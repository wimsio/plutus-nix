{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, FilePath, String, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

---
-- Datum & Redeemer
---

data EscrowDatum = EscrowDatum
    { edDepositor   :: PubKeyHash
    , edBeneficiary :: PubKeyHash
    , edOfficials   :: [PubKeyHash]   -- m officials
    , edApprovals   :: [PubKeyHash]   -- collected approvals
    , edRequired    :: Integer        -- n approvals required
    , edDeadline    :: POSIXTime
    }

PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction
    = Approve
    | Release
    | Refund

PlutusTx.unstableMakeIsData ''EscrowAction

---
-- Helpers
---

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE beforeDeadline #-}
beforeDeadline :: POSIXTime -> ScriptContext -> Bool
beforeDeadline dl ctx =
    contains (to dl) (txInfoValidRange $ scriptContextTxInfo ctx)

{-# INLINABLE afterDeadline #-}
afterDeadline :: POSIXTime -> ScriptContext -> Bool
afterDeadline dl ctx =
    contains (from dl) (txInfoValidRange $ scriptContextTxInfo ctx)

{-# INLINABLE uniqueApproval #-}
uniqueApproval :: PubKeyHash -> EscrowDatum -> Bool
uniqueApproval pkh d =
    not (elem pkh (edApprovals d)) &&
    elem pkh (edOfficials d)

---
-- Validator
---

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator d act ctx =
    case act of
        ----------------------------------------------------
        -- Approve: official signs once before deadline
        ----------------------------------------------------
        Approve ->
            let info = scriptContextTxInfo ctx
                signer =
                    case txInfoSignatories info of
                        [s] -> s
                        _   -> traceError "exactly one signer required"
            in
            traceIfFalse "deadline passed" (beforeDeadline (edDeadline d) ctx) &&
            traceIfFalse "invalid approver" (uniqueApproval signer d)

        ----------------------------------------------------
        -- Release: before deadline + approvals >= required
        ----------------------------------------------------
        Release ->
            traceIfFalse "deadline passed" (beforeDeadline (edDeadline d) ctx) &&
            traceIfFalse "not enough approvals"
                (length (edApprovals d) >= edRequired d) &&
            traceIfFalse "beneficiary signature missing"
                (signedBy (edBeneficiary d) ctx)

        ----------------------------------------------------
        -- Refund: after deadline + insufficient approvals
        ----------------------------------------------------
        Refund ->
            traceIfFalse "deadline not reached" (afterDeadline (edDeadline d) ctx) &&
            traceIfFalse "approvals already sufficient"
                (length (edApprovals d) < edRequired d) &&
            traceIfFalse "depositor signature missing"
                (signedBy (edDepositor d) ctx)

---
-- Boilerplate
---

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let datum = unsafeFromBuiltinData @EscrowDatum d
        red   = unsafeFromBuiltinData @EscrowAction r
        ctx   = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator datum red ctx then () else error ()

validator :: Validator
validator =
    mkValidatorScript
        $$(PlutusTx.compile [|| mkValidatorUntyped ||])

---
-- Address & Serialization
---

plutusValidatorHash :: Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes = Serialise.serialise v
        short = SBS.toShort (LBS.toStrict bytes)
    in PlutusV2.ValidatorHash (toBuiltin (SBS.fromShort short))

toBech32PolicyAddress :: C.NetworkId -> MintingPolicy -> String
toBech32PolicyAddress network pol =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise pol
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

---
-- Write Script
---

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

---
-- Main
---

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "multi-approval-escrow.plutus" validator
    putStrLn "\n--- Multi-Approval Escrow Validator ---"

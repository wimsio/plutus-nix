{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, flattenValue)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data SnapshotDatum = SnapshotDatum
    { sdPolicyId     :: CurrencySymbol   -- Policy ID being snapshot
    , sdBlock        :: POSIXTime        -- Block / snapshot time
    , sdGovernor     :: PubKeyHash       -- Who can tally / verify
    }
PlutusTx.unstableMakeIsData ''SnapshotDatum

data SnapshotAction = CreateSnapshot | VerifySnapshot
PlutusTx.unstableMakeIsData ''SnapshotAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE scriptInputHasPolicy #-}
scriptInputHasPolicy :: ScriptContext -> CurrencySymbol -> Bool
scriptInputHasPolicy ctx cs =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in any (\(cs', _tn, _amt) -> cs' == cs) (flattenValue v)


{-# INLINABLE txSignedByGovernor #-}
txSignedByGovernor :: ScriptContext -> PubKeyHash -> Bool
txSignedByGovernor ctx pk = txSignedBy (scriptContextTxInfo ctx) pk

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkSnapshotValidator #-}
mkSnapshotValidator :: SnapshotDatum -> SnapshotAction -> ScriptContext -> Bool
mkSnapshotValidator dat action ctx =
    case action of
        CreateSnapshot ->
            traceIfFalse "snapshot input missing policy" (scriptInputHasPolicy ctx (sdPolicyId dat)) &&
            traceIfFalse "only governor can create snapshot" (txSignedByGovernor ctx (sdGovernor dat))
        VerifySnapshot ->
            traceIfFalse "snapshot input missing policy" (scriptInputHasPolicy ctx (sdPolicyId dat)) &&
            traceIfFalse "verification must be after snapshot block" afterBlock
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterBlock :: Bool
    afterBlock = Interval.contains (Interval.from (sdBlock dat + 1)) txRange

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @SnapshotDatum d
        red = unsafeFromBuiltinData @SnapshotAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkSnapshotValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
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

------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "snapshot_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Snapshot Voting Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "--------------------------------------"
    putStrLn "Snapshot voting validator generated successfully."

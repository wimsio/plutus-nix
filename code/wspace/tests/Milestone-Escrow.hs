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
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
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

-------------------------------------------------------------------------------
-- Datum Types
-------------------------------------------------------------------------------

-- Campaign controls the entire funding campaign
-- creator = project owner
-- target  = required total ADA to fully fund
-- deadline = campaign deadline
-- raised = how much already pledged
-- milestoneIdx = which milestone creator can unlock next

data CampaignDatum = CampaignDatum
    { cdCreator      :: PubKeyHash
    , cdTarget       :: Integer
    , cdDeadline     :: POSIXTime
    , cdRaised       :: Integer
    , cdMilestoneIdx :: Integer
    }
PlutusTx.unstableMakeIsData ''CampaignDatum

-- Each backer stores their pledge amount

data PledgeDatum = PledgeDatum
    { pdBacker :: PubKeyHash
    , pdAmount :: Integer
    }
PlutusTx.unstableMakeIsData ''PledgeDatum

-- Redeemer for actions

data EscrowAction = Pledge | UnlockMilestone | Refund
PlutusTx.unstableMakeIsData ''EscrowAction

-------------------------------------------------------------------------------
-- Helper: Check signatures and timing
-------------------------------------------------------------------------------

{-# INLINABLE afterDeadline #-}
afterDeadline :: POSIXTime -> ScriptContext -> Bool
afterDeadline dl ctx = Interval.contains (Interval.from (dl + 1)) (txInfoValidRange info)
  where info = scriptContextTxInfo ctx

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

-------------------------------------------------------------------------------
-- Validator Logic
-------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator rawDatum rawRedeemer rawCtx =
    let dat = unsafeFromBuiltinData @CampaignDatum rawDatum
        red = unsafeFromBuiltinData @EscrowAction rawRedeemer
        ctx = unsafeFromBuiltinData @ScriptContext rawCtx
        info = scriptContextTxInfo ctx

        totalPledged :: Integer
        totalPledged = cdRaised dat

        targetReached :: Bool
        targetReached = totalPledged >= cdTarget dat

        mintedAda :: Integer
        mintedAda = valueOf (txInfoMint info) adaSymbol adaToken

    in case red of

        -----------------------------------------------------------------------
        -- PLEDGE
        -----------------------------------------------------------------------
        Pledge ->
            let deadlineOk = not (afterDeadline (cdDeadline dat) ctx)
            in  if traceIfFalse "too late" deadlineOk
                then
                    let newRaised      = mintedAda + totalPledged
                        pledgeIncrease = newRaised > totalPledged
                    in  if traceIfFalse "pledge must increase raised" pledgeIncrease
                        then ()
                        else ()
                else ()

        -----------------------------------------------------------------------
        -- UNLOCK MILESTONE
        -----------------------------------------------------------------------
        UnlockMilestone ->
            let creatorOk = signedBy (cdCreator dat) ctx
            in  if traceIfFalse "not creator" creatorOk
                then
                    if traceIfFalse "target not met" targetReached
                    then ()
                    else ()
                else ()

        -----------------------------------------------------------------------
        -- REFUND
        -----------------------------------------------------------------------
        Refund ->
            let afterOk = afterDeadline (cdDeadline dat) ctx
            in  if traceIfFalse "too early" afterOk
                then
                    let targetNotReached = not targetReached
                    in  if traceIfFalse "target reached" targetNotReached
                        then ()
                        else ()
                else ()


-------------------------------------------------------------------------------
-- Compile
-------------------------------------------------------------------------------

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

-------------------------------------------------------------------------------
-- On-Chain Hash + Plutus Address
-------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

-------------------------------------------------------------------------------
-- Bech32 Off-Chain Address
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- File Writing
-------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "milestone-escrow.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "--- Milestone Escrow Contract ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Script Address: " <> P.show onchain
    putStrLn $ "Bech32 Script Address: " <> bech32
    putStrLn "---------------------------------"

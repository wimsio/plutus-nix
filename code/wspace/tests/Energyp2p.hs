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

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data EnergyDatum = EnergyDatum
    { edProducer      :: PubKeyHash
    , edConsumer      :: PubKeyHash
    , edKWhAmount     :: Integer           -- kWh token amount
    , edCarbonToken   :: CurrencySymbol    -- Carbon token policy
    , edCarbonTN      :: TokenName         -- Carbon token
    , edDeadline      :: POSIXTime         -- Settlement deadline
    }
PlutusTx.unstableMakeIsData ''EnergyDatum

data EnergyAction = PayProducer | RetireCarbon
PlutusTx.unstableMakeIsData ''EnergyAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE scriptInputHasToken #-}
scriptInputHasToken :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputHasToken ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EnergyDatum -> EnergyAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        PayProducer ->
            traceIfFalse "script input missing kWh token" (scriptInputHasToken ctx (edCarbonToken dat) (edCarbonTN dat)) &&
            traceIfFalse "consumer signature missing" (txSignedBy info (edConsumer dat)) &&
            traceIfFalse "producer not paid" producerPaid
        RetireCarbon ->
            traceIfFalse "carbon token missing" (scriptInputHasToken ctx (edCarbonToken dat) (edCarbonTN dat)) &&
            traceIfFalse "producer signature missing" (txSignedBy info (edProducer dat)) &&
            traceIfFalse "retire after deadline" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange

    producerPaid :: Bool
    producerPaid =
      let v = valuePaidTo info (edProducer dat)
      in valueOf v adaSymbol adaToken >= edKWhAmount dat

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EnergyDatum d
        red = unsafeFromBuiltinData @EnergyAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

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

    writeValidator "energy_validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- P2P Energy & Carbon Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-----------------------------------------"
    putStrLn "Energy / Carbon smart contract generated successfully."

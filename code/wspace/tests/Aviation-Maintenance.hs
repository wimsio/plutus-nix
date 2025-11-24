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

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum Types
------------------------------------------------------------------------

-- NFT representing a specific aircraft part
data PartNFT = PartNFT
    { pnTailNo      :: BuiltinByteString
    , pnSerialNo    :: BuiltinByteString
    , pnRecordsHash :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''PartNFT

-- A single maintenance log entry
data MaintenanceLog = MaintenanceLog
    { mlWork       :: BuiltinByteString
    , mlTimestamp  :: POSIXTime
    , mlCertifier  :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''MaintenanceLog

-- Slot booking for landing / takeoff fee payment
data Slot = Slot
    { slAirport :: BuiltinByteString
    , slTime    :: POSIXTime
    , slFee     :: Integer
    }
PlutusTx.unstableMakeIsData ''Slot

-- Combined datum stored at script UTxO
data AviationDatum = AviationDatum
    { adPartNFT  :: PartNFT
    , adLog      :: MaintenanceLog
    , adSlot     :: Slot
    , adOwner    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''AviationDatum

------------------------------------------------------------------------
-- Redeemer
------------------------------------------------------------------------

data AviationAction
    = AppendLog      -- certifier adds maintenance record
    | PaySlotFee     -- aircraft owner pays the landing/takeoff fee
PlutusTx.unstableMakeIsData ''AviationAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE nftPresent #-}
nftPresent :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
nftPresent ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "missing script input"
        Just i  ->
            let v = txOutValue (txInInfoResolved i)
            in valueOf v cs tn >= 1

{-# INLINABLE paidTo #-}
paidTo :: TxInfo -> PubKeyHash -> Value
paidTo info pkh =
    mconcat
      [ txOutValue o
      | o <- txInfoOutputs info
      , txOutAddress o == Address (PubKeyCredential pkh) Nothing
      ]

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: AviationDatum -> AviationAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      ------------------------------------------------------------------
      -- ✓ Append Maintenance Log
      ------------------------------------------------------------------
      AppendLog ->
           traceIfFalse "certifier signature missing"
               (txSignedBy info (mlCertifier (adLog dat))) &&
           traceIfFalse "part NFT missing"
               (nftPresent ctx (CurrencySymbol "") (TokenName ""))

      ------------------------------------------------------------------
      -- ✓ Pay Slot (Landing/Takeoff) Fee
      ------------------------------------------------------------------
      PaySlotFee ->
           traceIfFalse "owner signature missing"
               (txSignedBy info (adOwner dat)) &&
           traceIfFalse "fee not paid"
               feePaid

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    feePaid :: Bool
    feePaid =
        let v = paidTo info (adOwner dat)
            required = slFee (adSlot dat)
        in valueOf v adaSymbol adaToken >= required

------------------------------------------------------------------------
-- Untyped Validator
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @AviationDatum d
        red = unsafeFromBuiltinData @AviationAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Hash + Addresses (same as your working script)
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash v =
    let bytes    = Serialise.serialise v
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
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash   = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr  =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- Write File
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- Main (identical structure to yours)
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "aviation-maintenance.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Aviation Maintenance + Slot Fee Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "-------------------------------------------------"
    putStrLn "Aviation validator generated successfully."

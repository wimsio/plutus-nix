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
import qualified Data.ByteString.Base16 as B16
import qualified Data.Text.Encoding as TE

-- Cardano API (for Bech32)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Datum + Redeemer
------------------------------------------------------------------------

data EscrowDatum = EscrowDatum
    { edBuyer    :: PubKeyHash
    , edSeller   :: PubKeyHash
    , edAmount   :: Integer
    , edDeadline :: POSIXTime
    , edCurrency :: CurrencySymbol
    , edToken    :: TokenName
    }
PlutusTx.unstableMakeIsData ''EscrowDatum

data EscrowAction = PaySeller | RefundSeller
PlutusTx.unstableMakeIsData ''EscrowAction

------------------------------------------------------------------------
-- Helper
------------------------------------------------------------------------

{-# INLINABLE scriptInputContainsNFT #-}
scriptInputContainsNFT :: ScriptContext -> CurrencySymbol -> TokenName -> Bool
scriptInputContainsNFT ctx cs tn =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in valueOf v cs tn >= 1

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EscrowDatum -> EscrowAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
      PaySeller ->
           traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "buyer signature missing"   (txSignedBy info (edBuyer dat)) &&
           traceIfFalse "seller not paid"           sellerPaid &&
           traceIfFalse "buyer not receive NFT"     buyerGetsNFT

      RefundSeller ->
           traceIfFalse "script input missing NFT" (scriptInputContainsNFT ctx (edCurrency dat) (edToken dat)) &&
           traceIfFalse "seller signature missing" (txSignedBy info (edSeller dat)) &&
           traceIfFalse "too early for refund"     afterDeadline &&
           traceIfFalse "seller did not receive NFT" sellerGetsNFT

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txRange :: POSIXTimeRange
    txRange = txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = Interval.contains (Interval.from (edDeadline dat + 1)) txRange

    sellerPaid :: Bool
    sellerPaid =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v adaSymbol adaToken >= edAmount dat

    buyerGetsNFT :: Bool
    buyerGetsNFT =
      let v = valuePaidTo info (edBuyer dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

    sellerGetsNFT :: Bool
    sellerGetsNFT =
      let v = valuePaidTo info (edSeller dat)
      in valueOf v (edCurrency dat) (edToken dat) >= 1

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @EscrowDatum d
        red = unsafeFromBuiltinData @EscrowAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- On-chain hash + address
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress =
    Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------
-- Bech32 address (off-chain)
------------------------------------------------------------------------

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
-- CBOR HEX Generator
------------------------------------------------------------------------

validatorToCborHex :: Validator -> String
validatorToCborHex val =
    let cbor = LBS.toStrict (Serialise.serialise val)
        hex  = B16.encode cbor
    in T.unpack (TE.decodeUtf8 hex)

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR fp val = do
    let hex = validatorToCborHex val
    BS.writeFile fp (TE.encodeUtf8 (T.pack hex))
    putStrLn ("CBOR hex written to: " <> fp)

------------------------------------------------------------------------
-- Write Raw Plutus
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

    -- Write binary Plutus script
    writeValidator "validator.plutus" validator

    -- Write CBOR hex
    writeCBOR "validator.cbor" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Escrow NFT Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Escrow NFT validator generated successfully."

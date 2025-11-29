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

-- Plutus Ledger
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts 
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS
import qualified PlutusTx.Builtins as Builtins

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------------------------------------
-- DATUM & REDEEMERS
-------------------------------------------------------------------------------

data RemitDatum = RemitDatum
    { rdSender      :: PubKeyHash
    , rdReceiver    :: PubKeyHash
    , rdAmount      :: Integer
    , rdCorridor    :: BuiltinByteString
    , rdFxOracleRef :: TxOutRef
    , rdKycRef      :: TxOutRef
    }
PlutusTx.unstableMakeIsData ''RemitDatum

data RemitAction = Deposit | Withdraw
PlutusTx.unstableMakeIsData ''RemitAction

-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------

{-# INLINABLE hasRef #-}
hasRef :: TxInfo -> TxOutRef -> Bool
hasRef info expectedRef =
    any (\i -> txInInfoOutRef i == expectedRef) (txInfoInputs info)

{-# INLINABLE valuePaidToPKH #-}
valuePaidToPKH :: TxInfo -> PubKeyHash -> Value
valuePaidToPKH info pkh =
    mconcat
        [ txOutValue o
        | o <- txInfoOutputs info
        , txOutAddress o == pubKeyHashAddress pkh
        ]

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress p = Address (PubKeyCredential p) Nothing

-------------------------------------------------------------------------------
-- FX ORACLE PARSING
-------------------------------------------------------------------------------

{-# INLINABLE readOracleRate #-}
readOracleRate :: TxOut -> Integer
readOracleRate o =
    case txOutDatum o of
        OutputDatum (Datum d) ->
            unsafeFromBuiltinData @Integer d
        _ -> traceError "oracle must have inline datum"

{-# INLINABLE getOracleOutput #-}
getOracleOutput :: TxInfo -> TxOutRef -> TxOut
getOracleOutput info ref =
    case [ txInInfoResolved i | i <- txInfoInputs info, txInInfoOutRef i == ref ] of
        [o] -> o
        _   -> traceError "oracle input missing"

-------------------------------------------------------------------------------
-- VALIDATOR
-------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: RemitDatum -> RemitAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of

      --------------------------------------------------------------------------
      -- SENDER DEPOSIT FLOW
      --------------------------------------------------------------------------
      Deposit ->
           traceIfFalse "sender sig missing" (txSignedBy info (rdSender dat))              &&
           traceIfFalse "oracle ref missing" (hasRef info (rdFxOracleRef dat))             &&
           traceIfFalse "kyc ref missing"    (hasRef info (rdKycRef dat))                  &&
           traceIfFalse "deposit < required" senderDepositedEnough

      --------------------------------------------------------------------------
      -- RECEIVER WITHDRAWAL FLOW
      --------------------------------------------------------------------------
      Withdraw ->
           traceIfFalse "receiver sig missing" (txSignedBy info (rdReceiver dat))          &&
           traceIfFalse "oracle ref missing"   (hasRef info (rdFxOracleRef dat))           &&
           traceIfFalse "kyc ref missing"      (hasRef info (rdKycRef dat))                &&
           traceIfFalse "receiver underpaid"   receiverPaidCorrect

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    --------------------------------------------------------------------------
    -- Sender must deposit ≥ rdAmount (in ADA for now)
    --------------------------------------------------------------------------
    senderDepositedEnough :: Bool
    senderDepositedEnough =
        let v = valuePaidToPKH info (rdSender dat)
        in valueOf v adaSymbol adaToken <= negate (rdAmount dat)

    --------------------------------------------------------------------------
    -- Receiver must receive FX-adjusted amount
    --------------------------------------------------------------------------
    receiverPaidCorrect :: Bool
    receiverPaidCorrect =
        let oracleOut  = getOracleOutput info (rdFxOracleRef dat)
            rate       = readOracleRate oracleOut
            adjusted   = (rdAmount dat * rate) `divide` 1000
            v          = valuePaidToPKH info (rdReceiver dat)
        in valueOf v adaSymbol adaToken >= adjusted

-------------------------------------------------------------------------------
-- UNTYPED
-------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @RemitDatum d
        red = unsafeFromBuiltinData @RemitAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------------------------------------
-- VALIDATOR HASH + ADDRESSES (same format as your working script)
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- WRITE FILE
-------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

-------------------------------------------------------------------------------
-- MAIN  (same structure as your escrow script)
-------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    -------------------------------------------------------
    -- Write validator to file
    -------------------------------------------------------
    writeValidator "cross-border-remit.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    -------------------------------------------------------
    -- Display validator info
    -------------------------------------------------------
    putStrLn "\n--- Cross-Border Remittance Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "------------------------------------------------"
    putStrLn "Cross-border remittance validator generated."

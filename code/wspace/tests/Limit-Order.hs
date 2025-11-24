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
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins
import Plutus.V1.Ledger.Interval as Interval

-- Serialization
import qualified Codec.Serialise      as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString      as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- Datum + Redeemer
--------------------------------------------------------------------------------

data OrderDatum = OrderDatum
    { odOwner     :: PubKeyHash
    , odBase      :: CurrencySymbol
    , odQuote     :: CurrencySymbol
    , odSide      :: Bool            -- True = BUY, False = SELL
    , odLimitNum  :: Integer
    , odLimitDen  :: Integer
    , odQty       :: Integer
    , odRemaining :: Integer
    }
PlutusTx.unstableMakeIsData ''OrderDatum

data OrderAction
    = Fill Integer Integer Integer    -- amount, execNum, execDen
    | Cancel
PlutusTx.unstableMakeIsData ''OrderAction

--------------------------------------------------------------------------------
-- Price-check helper
--------------------------------------------------------------------------------

{-# INLINABLE priceOK #-}
priceOK :: Bool -> Integer -> Integer -> Integer -> Integer -> Bool
priceOK side limitN limitD execN execD =
    if side
       then execN * limitD <= limitN * execD       -- BUY: taker price <= limit
       else execN * limitD >= limitN * execD       -- SELL: taker price >= limit

--------------------------------------------------------------------------------
-- Validator logic
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: OrderDatum -> OrderAction -> ScriptContext -> Bool
mkValidator dat act ctx =
    case act of

      -- ------------------------------------------------------------
      -- TAKERS FILL ORDER
      -- ------------------------------------------------------------
      Fill amount execN execD ->
           traceIfFalse "remaining insufficient" (amount <= odRemaining dat)
        && traceIfFalse "bad price" (priceOK (odSide dat) (odLimitNum dat) (odLimitDen dat) execN execD)
        && traceIfFalse "must update remaining"
             (checkRemainingUpdate amount)

      -- ------------------------------------------------------------
      -- OWNER CANCELS
      -- ------------------------------------------------------------
      Cancel ->
           traceIfFalse "owner sig missing" (txSignedBy info (odOwner dat))

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput =
      case findOwnInput ctx of
        Nothing -> traceError "missing input"
        Just i  -> txInInfoResolved i

    newOutput :: TxOut
    newOutput =
      case getContinuingOutputs ctx of
        [o] -> o
        []  -> traceError "expected continuing output"
        _   -> traceError "too many outputs"

    -- Check remaining is decremented by amount
    checkRemainingUpdate :: Integer -> Bool
    checkRemainingUpdate amount =
      case txOutDatum newOutput of
        OutputDatum d ->
            case fromBuiltinData (getDatum d) of
              Just newDat ->
                   odRemaining newDat == odRemaining dat - amount
              Nothing -> traceError "bad datum"
        _ -> traceError "expected output datum"

--------------------------------------------------------------------------------
-- Untyped wrapper
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @OrderDatum d
        red = unsafeFromBuiltinData @OrderAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Hash + Address
--------------------------------------------------------------------------------

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

        -- REQUIRED LINE (your preference)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress

    in T.unpack (C.serialiseAddress shelleyAddr)

--------------------------------------------------------------------------------
-- File writing
--------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "limit-order.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Limit Order Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Script Address: " <> P.show onchain
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "----------------------------------"
    putStrLn "Limit order validator generated successfully."

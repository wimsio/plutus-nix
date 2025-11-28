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
-- Datum & Redeemer
------------------------------------------------------------------------

data LineDatum = LineDatum
    { ldBorrower    :: PubKeyHash
    , ldLimit       :: Integer        -- max draw
    , ldDrawn       :: Integer        -- currently drawn
    , ldRateModel   :: BuiltinByteString -- params for interest rate model
    , ldLastAccrual :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''LineDatum

data LineAction
    = Draw Integer          -- amount to draw
    | Repay Integer         -- amount to repay
    | AdjustLimit Integer   -- new limit, requires controller sig
    | Accrue                -- apply interest to drawn
PlutusTx.unstableMakeIsData ''LineAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE elapsed #-}
elapsed :: POSIXTime -> POSIXTime -> Integer
elapsed last now = getPOSIXTime now - getPOSIXTime last

{-# INLINABLE accrueInterest #-}
accrueInterest :: LineDatum -> POSIXTime -> LineDatum
accrueInterest ld now =
    let delta = elapsed (ldLastAccrual ld) now
        rate  = 5 -- 5% per unit time for simplicity
        interest = divide (ldDrawn ld * rate * delta) 100
    in ld { ldDrawn = ldDrawn ld + interest, ldLastAccrual = now }

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: LineDatum -> LineAction -> ScriptContext -> Bool
mkValidator ld act ctx =
    case act of
        Draw amt ->
            traceIfFalse "draw exceeds limit" ((ldDrawn ld + amt) <= ldLimit ld) &&
            traceIfFalse "borrower signature missing" (signedBy (ldBorrower ld) ctx)
        Repay amt ->
            traceIfFalse "repay exceeds drawn" (amt <= ldDrawn ld) &&
            traceIfFalse "borrower signature missing" (signedBy (ldBorrower ld) ctx)
        AdjustLimit newLimit ->
            traceIfFalse "new limit must be >= drawn" (newLimit >= ldDrawn ld) &&
            traceIfFalse "controller signature missing" (signedBy controller ctx)
        Accrue ->
            traceIfFalse "accrual only" True
  where
    info = scriptContextTxInfo ctx
    controller = ldBorrower ld -- replace with governance/treasury pkh if needed

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let ld  = unsafeFromBuiltinData @LineDatum d
        act = unsafeFromBuiltinData @LineAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator ld act ctx then () else error ()

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
    writeValidator "revolving-credit-line.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Revolving Credit Line Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Revolving credit line validator generated successfully."

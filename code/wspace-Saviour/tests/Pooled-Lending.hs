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

--------------------------------------------------------------------------------
-- Pool Datum & Redeemer
--------------------------------------------------------------------------------

data PoolDatum = PoolDatum
    { pdCash         :: Integer       -- total ADA in pool
    , pdBorrows      :: Integer       -- total borrows
    , pdReserveFactor :: Integer      -- e.g., 10 = 10%
    , pdRateModel    :: BuiltinByteString -- parameters for interest rate curve
    }
PlutusTx.unstableMakeIsData ''PoolDatum

data PoolAction
    = Deposit Integer        -- deposit amount
    | Withdraw Integer       -- withdraw amount
    | Borrow Integer         -- borrow amount
    | Repay Integer          -- repay amount
    | Accrue                 -- update interest / reserves
PlutusTx.unstableMakeIsData ''PoolAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE after #-}
after :: POSIXTime -> ScriptContext -> Bool
after t ctx = Interval.contains (Interval.from t) (txInfoValidRange $ scriptContextTxInfo ctx)

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

-- Utilization ratio = borrows / (cash + borrows)
{-# INLINABLE utilization #-}
utilization :: PoolDatum -> Integer
utilization pd = if pdCash pd + pdBorrows pd == 0 then 0 else divide (pdBorrows pd * 100) (pdCash pd + pdBorrows pd)

-- Simple interest accrual
{-# INLINABLE accrueInterest #-}
accrueInterest :: PoolDatum -> PoolDatum
accrueInterest pd =
    let rate = 5 -- 5% per epoch for simplicity
        interest = divide (pdBorrows pd * rate) 100
        reserve = divide (interest * pdReserveFactor pd) 100
    in pd { pdBorrows = pdBorrows pd + interest - reserve
          , pdCash = pdCash pd + reserve
          }

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PoolDatum -> PoolAction -> ScriptContext -> Bool
mkValidator pd act ctx =
    case act of
        Deposit amt ->
            traceIfFalse "deposit must be >0" (amt > 0)
        Withdraw amt ->
            traceIfFalse "withdraw exceeds pool" (amt <= pdCash pd)
        Borrow amt ->
            traceIfFalse "borrow exceeds utilization limit" (pdBorrows pd + amt <= maxBorrow)
        Repay amt ->
            traceIfFalse "repay > borrows" (amt <= pdBorrows pd)
        Accrue ->
            True
  where
    info = scriptContextTxInfo ctx
    maxBorrow = divide ((pdCash pd + pdBorrows pd) * 80) 100 -- 80% max utilization

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let pd = unsafeFromBuiltinData @PoolDatum d
        act = unsafeFromBuiltinData @PoolAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator pd act ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Plutus Hash + Addresses
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
    writeValidator "pool-lending.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Pooled Lending Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Pooled lending validator generated successfully."

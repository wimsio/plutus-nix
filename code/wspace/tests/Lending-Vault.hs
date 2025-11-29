{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------
-- Vault Datum and Redeemer
------------------------------------------------------------------------

data VaultDatum = VaultDatum
    { vdOwner    :: PubKeyHash
    , vdCollValue :: Integer
    , vdDebt     :: Integer
    , vdMCR      :: Integer  -- Minimum Collateral Ratio, e.g., 150%
    , vdLCR      :: Integer  -- Liquidation Collateral Ratio, e.g., 120%
    }
PlutusTx.unstableMakeIsData ''VaultDatum

data VaultAction = Open | Draw Integer | Repay Integer | Close | Liquidate PubKeyHash
PlutusTx.unstableMakeIsData ''VaultAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE after #-}
after :: POSIXTime -> ScriptContext -> Bool
after t ctx = Interval.contains (Interval.from (t + 1)) (txInfoValidRange $ scriptContextTxInfo ctx)

{-# INLINABLE collateralRatio #-}
collateralRatio :: Integer -> Integer -> Integer
collateralRatio coll debt = if debt == 0 then 100000 else (coll * 100) `divide` debt

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkVaultValidator #-}
mkVaultValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkVaultValidator dat action ctx =
    case action of
        Open -> traceIfFalse "must be signed by owner" signedOwner
        Draw amt -> traceIfFalse "under MCR" postOpMCR && traceIfFalse "signed by owner" signedOwner
            where
                postOpMCR = collateralRatio (vdCollValue dat) (vdDebt dat + amt) >= vdMCR dat
        Repay amt -> traceIfFalse "signed by owner" signedOwner && traceIfFalse "collateral ratio ok" postOpMCR
            where
                newDebt = vdDebt dat - amt
                postOpMCR = collateralRatio (vdCollValue dat) newDebt >= vdMCR dat || newDebt == 0
        Close -> traceIfFalse "signed by owner" signedOwner && traceIfFalse "no debt remaining" noDebt
            where
                noDebt = vdDebt dat == 0
        Liquidate liquidator -> traceIfFalse "not under LCR" underLCR && traceIfFalse "liquidator signed" signedByLiquidator
            where
                ratio = collateralRatio (vdCollValue dat) (vdDebt dat)
                underLCR = ratio < vdLCR dat
                signedByLiquidator = signedBy liquidator ctx
  where
    signedOwner = signedBy (vdOwner dat) ctx

------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkVaultValidator (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c) then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Validator Hash + Addresses
------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------
-- Off-chain Bech32 address
------------------------------------------------------------------------

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

------------------------------------------------------------------------
-- File Writing
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
    writeValidator "vault.plutus" validator
    let vh = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator
    putStrLn "\n--- CDP Lending Vault Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Script Address: " <> P.show onchain
    putStrLn $ "Bech32 Script Address: " <> bech32
    putStrLn "---------------------------------"
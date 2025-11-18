{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T
import Data.Maybe (listToMaybe)

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

import GHC.Generics (Generic)

-------------------------------------------------
-- On-chain datatypes
-------------------------------------------------

data VaultDatum = VaultDatum
    { vdOwner :: PubKeyHash
    , vdColl  :: Integer  -- collateral in lovelace
    , vdDebt  :: Integer  -- debt in sASSET units
    , vdMCR   :: Integer  -- minimum collateral ratio %
    }
    deriving (Generic)
PlutusTx.unstableMakeIsData ''VaultDatum

data OracleDatum = OracleDatum
    { odPrice  :: Integer   -- price in lovelace per sASSET
    , odTs     :: POSIXTime
    , odSigner :: PubKeyHash
    }
    deriving (Generic)
PlutusTx.unstableMakeIsData ''OracleDatum

data VaultAction = Draw | Repay | Liquidate
PlutusTx.unstableMakeIsData ''VaultAction

-------------------------------------------------
-- Validator logic
-------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        Draw      -> traceIfFalse "owner signature missing" (txSignedBy info (vdOwner dat))
        Repay     -> traceIfFalse "owner signature missing" (txSignedBy info (vdOwner dat))
        Liquidate -> traceIfFalse "too healthy to liquidate" (collateralRatio dat < vdMCR dat)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    collateralRatio :: VaultDatum -> Integer
    collateralRatio v =
        if vdDebt v == 0 then 1000 else (vdColl v * 100) `divide` vdDebt v

    divide :: Integer -> Integer -> Integer
    divide a b = a `PlutusTx.Prelude.divide` b

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @VaultDatum d
        red = unsafeFromBuiltinData @VaultAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------
-- Validator Hash + Addresses
-------------------------------------------------

plutusValidatorHash :: Validator -> ValidatorHash
plutusValidatorHash val =
    let bytes   = Serialise.serialise val           -- Lazy ByteString
        strict  = LBS.toStrict bytes               -- Strict ByteString
        builtin = Builtins.toBuiltin strict        -- BuiltinByteString
    in ValidatorHash builtin


plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra
            network
            (C.PaymentCredentialByScript scriptHash)
            C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------
-- File writing
-------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

-------------------------------------------------
-- Main
-------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "vault-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Vault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Vault validator generated successfully."

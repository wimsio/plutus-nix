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
import Plutus.V1.Ledger.Interval as Interval hiding (singleton)
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken, geq)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- Datum and Redeemer
--------------------------------------------------------------------------------

data VaultDatum = VaultDatum
    { vdOwner  :: PubKeyHash
    , vdColl   :: Integer        -- collateral locked (in lovelace or custom token)
    , vdDebt   :: Integer        -- stablecoin debt minted
    , vdMCR    :: Integer        -- minimum collateral ratio in %
    , vdRateIx :: Integer        -- interest/stability fee index
    }
PlutusTx.unstableMakeIsData ''VaultDatum

data VaultAction = Open | Draw Integer | Repay Integer | Liquidate
PlutusTx.unstableMakeIsData ''VaultAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE collateralRatio #-}
collateralRatio :: VaultDatum -> Bool
collateralRatio vd =
    vdColl vd * 100 >= vdDebt vd * vdMCR vd
-- Note: integer % check; adjust precision if needed

{-# INLINABLE scriptInputContainsCollateral #-}
scriptInputContainsCollateral :: ScriptContext -> Integer -> Bool
scriptInputContainsCollateral ctx required =
    case findOwnInput ctx of
        Nothing -> traceError "no input from script found"
        Just i  ->
            let v = txOutValue $ txInInfoResolved i
            in v `geq` singleton adaSymbol adaToken required

--------------------------------------------------------------------------------
-- Validator Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkVaultValidator #-}
mkVaultValidator :: VaultDatum -> VaultAction -> ScriptContext -> Bool
mkVaultValidator vd act ctx =
    case act of
        Open          -> traceIfFalse "collateral ratio too low" (collateralRatio vd)
        Draw amt      -> let vd' = vd { vdDebt = vdDebt vd + amt }
                         in traceIfFalse "draw exceeds collateral" (collateralRatio vd')
        Repay amt     -> let vd' = vd { vdDebt = vdDebt vd - amt }
                         in traceIfFalse "repay leaves negative debt" (vdDebt vd' >= 0)
        Liquidate     -> traceIfFalse "collateral healthy" (not $ collateralRatio vd)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE mkVaultValidatorUntyped #-}
mkVaultValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkVaultValidatorUntyped d r c =
    let vd  = unsafeFromBuiltinData @VaultDatum d
        act = unsafeFromBuiltinData @VaultAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkVaultValidator vd act ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkVaultValidatorUntyped ||])

--------------------------------------------------------------------------------
-- Validator Hash + Addresses
--------------------------------------------------------------------------------

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

    writeValidator "vault.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Vault Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Over-collateralized stablecoin vault generated successfully."

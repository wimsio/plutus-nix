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

-----------------------------------------------------------------------------------
-- Synthetic Asset: Vault Datum
-----------------------------------------------------------------------------------

data SynthVault = SynthVault
    { svOwner      :: PubKeyHash
    , svCollateral :: Integer     -- ADA locked
    , svDebt       :: Integer     -- sASSET minted
    , svMinRatio   :: Integer     -- e.g. 150% = 150
    }
PlutusTx.unstableMakeIsData ''SynthVault

-----------------------------------------------------------------------------------
-- Oracle Reference Data
-----------------------------------------------------------------------------------

data PriceDatum = PriceDatum
    { pdPrice   :: Integer        -- price in lovelace per index unit
    , pdOracle  :: PubKeyHash     -- oracle signer
    , pdUpdated :: POSIXTime      -- timestamp
    }
PlutusTx.unstableMakeIsData ''PriceDatum

-----------------------------------------------------------------------------------
-- Redeemer – Synth Actions
-----------------------------------------------------------------------------------

data SynthAction = MintSynth Integer
                 | BurnSynth Integer
                 | UpdatePrice
                 | Liquidate
PlutusTx.unstableMakeIsData ''SynthAction

-----------------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------------

{-# INLINABLE getRefOracle #-}
getRefOracle :: ScriptContext -> PriceDatum
getRefOracle ctx =
    case findDatum' of
      Just d  -> d
      Nothing -> traceError "no oracle ref input"
  where
    info = scriptContextTxInfo ctx
    findDatum' =
        case txInfoReferenceInputs info of
            [ref] ->
                case txOutDatum (txInInfoResolved ref) of
                    OutputDatum d -> Just (unsafeFromBuiltinData @PriceDatum (getDatum d))
                    _             -> Nothing
            _ -> Nothing

{-# INLINABLE ratioOK #-}
ratioOK :: Integer -> Integer -> Integer -> Bool
ratioOK collateral debt minRatio =
    if debt == 0 then True else collateral * 100 >= debt * minRatio

-----------------------------------------------------------------------------------
-- Validator Logic
-----------------------------------------------------------------------------------
{-# INLINABLE lowerBoundTime #-}
lowerBoundTime :: POSIXTimeRange -> POSIXTime
lowerBoundTime r =
    case ivFrom r of
        LowerBound (Finite t) _ -> t
        _                       -> traceError "invalid lower bound"

{-# INLINABLE mkValidator #-}
mkValidator :: SynthVault -> SynthAction -> ScriptContext -> Bool
mkValidator vault action ctx =
    let info = scriptContextTxInfo ctx
        oracle = getRefOracle ctx
        now = lowerBoundTime (txInfoValidRange info)
        fresh = pdUpdated oracle + 30000 >= now
    in
    traceIfFalse "stale oracle" fresh &&
    case action of

      ----------------------------------------------------------------------------
      -- Mint new sASSET
      ----------------------------------------------------------------------------
      MintSynth amount ->
           traceIfFalse "owner sig required" (txSignedBy info (svOwner vault)) &&
           let newVault = getOutputDatum
               price = pdPrice oracle
               newDebt = svDebt vault + amount
               reqCollateral = (newDebt * price * svMinRatio vault) `divide` 100
           in
                traceIfFalse "ratio violation" (ratioOK (svCollateral newVault) newDebt (svMinRatio vault)) &&
                traceIfFalse "wrong new debt" (svDebt newVault == newDebt)

      ----------------------------------------------------------------------------
      -- Burn sASSET
      ----------------------------------------------------------------------------
      BurnSynth amount ->
           traceIfFalse "owner sig required" (txSignedBy info (svOwner vault)) &&
           let newVault = getOutputDatum
               newDebt  = svDebt vault - amount
           in traceIfFalse "wrong new debt" (svDebt newVault == newDebt)

      ----------------------------------------------------------------------------
      -- Update price oracle
      ----------------------------------------------------------------------------
      UpdatePrice ->
           traceIfFalse "oracle must sign" (txSignedBy info (pdOracle oracle))

      ----------------------------------------------------------------------------
      -- Liquidation flow
      ----------------------------------------------------------------------------
      Liquidate ->
           let price = pdPrice oracle
               ratioBad = not (ratioOK (svCollateral vault) (svDebt vault) (svMinRatio vault))
           in traceIfFalse "cannot liquidate (ratio ok)" ratioBad

  where
    info = scriptContextTxInfo ctx

    ----------------------------------------------------------------------------
    -- Read new vault datum from output
    ----------------------------------------------------------------------------
    getOutputDatum :: SynthVault
    getOutputDatum =
        case getContinuingOutputs ctx of
          [o] ->
             case txOutDatum o of
               OutputDatum d -> unsafeFromBuiltinData @SynthVault (getDatum d)
               _             -> traceError "bad datum format"
          _ -> traceError "expected 1 continuing output"

-----------------------------------------------------------------------------------
-- Boilerplate
-----------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @SynthVault d
        red = unsafeFromBuiltinData @SynthAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-----------------------------------------------------------------------------------
-- Hash + Addresses
-----------------------------------------------------------------------------------

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
    let
        serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val

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


-----------------------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "synth-validator.plutus" validator

    let vh     = plutusValidatorHash validator
        addr   = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- Synthetic Asset Validator Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "Plutus Address: " <> P.show addr
    putStrLn $ "Bech32 Address: " <> bech32
    putStrLn "---------------------------------------"
    putStrLn "Synthetic-asset validator generated successfully."

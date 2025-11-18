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

-- Plutus
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..))
import qualified PlutusTx.Builtins as Builtins
import qualified PlutusTx.AssocMap as Map

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS


--------------------------------------------------------------------------------
-- Datum / Redeemer (Rational replaced with scaled Integer)
--------------------------------------------------------------------------------

data RebaseDatum = RebaseDatum
    { rdGovernors :: [PubKeyHash]
    , rdFactor    :: Integer   -- scaled (e.g. 1.05 → 1050000)
    , rdMin       :: Integer
    , rdMax       :: Integer
    }
PlutusTx.unstableMakeIsData ''RebaseDatum

data RebaseAction
    = Rebase Integer        -- scaled factor
    | MintSome Integer
    | BurnSome Integer
PlutusTx.unstableMakeIsData ''RebaseAction


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE isGovernor #-}
isGovernor :: TxInfo -> [PubKeyHash] -> Bool
isGovernor info govs = any (\g -> txSignedBy info g) govs

{-# INLINABLE extractDatum #-}
extractDatum :: TxOut -> BuiltinData
extractDatum o = case txOutDatum o of
    NoOutputDatum        -> traceError "no datum"
    OutputDatum (Datum d) -> d
    OutputDatumHash _    -> traceError "datum hash unsupported"

{-# INLINABLE findController #-}
findController :: ScriptContext -> RebaseDatum
findController ctx =
    case [ d
         | i <- txInfoReferenceInputs info
         , let o = txInInfoResolved i
         , Just d <- [fromBuiltinData @RebaseDatum (extractDatum o)]
         ] of
        [d] -> d
        _   -> traceError "controller ref missing/duplicated"
  where
    info = scriptContextTxInfo ctx


--------------------------------------------------------------------------------
-- Minting Policy
--------------------------------------------------------------------------------

{-# INLINABLE mkPolicy #-}
mkPolicy :: RebaseAction -> ScriptContext -> Bool
mkPolicy redeemer ctx =
    let info = scriptContextTxInfo ctx
        controller = findController ctx
        govs = rdGovernors controller
    in case redeemer of

        ------------------------------------------------------------------------
        -- REBASE (governors only)
        ------------------------------------------------------------------------
        Rebase factor ->
            traceIfFalse "not governor" (isGovernor info govs) &&
            traceIfFalse "factor OOB"
                (factor >= rdMin controller && factor <= rdMax controller)

        ------------------------------------------------------------------------
        -- MINT (governors only)
        ------------------------------------------------------------------------
        MintSome amt ->
            traceIfFalse "not governor" (isGovernor info govs) &&
            traceIfFalse "bad amt" (amt > 0) &&
            traceIfFalse "mint mismatch" (checkMint info amt)

        ------------------------------------------------------------------------
        -- BURN (anyone may burn)
        ------------------------------------------------------------------------
        BurnSome amt ->
            traceIfFalse "bad burn amt" (amt > 0) &&
            traceIfFalse "burn mismatch" (checkBurn info amt)


--------------------------------------------------------------------------------
-- Mint / Burn checks
--------------------------------------------------------------------------------
{-# INLINABLE flattenValue #-}
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
flattenValue (Value m) =
    concatMap (\(cs, mp) -> [(cs, tn, amt) | (tn, amt) <- Map.toList mp]) (Map.toList m)

{-# INLINABLE checkMint #-}
checkMint :: TxInfo -> Integer -> Bool
checkMint info amt =
    case flattenValue (txInfoMint info) of
    [(cs, tn, q)] -> q == amt
    _             -> traceError "unexpected mint"

{-# INLINABLE checkBurn #-}
checkBurn :: TxInfo -> Integer -> Bool
checkBurn info amt =
    case flattenValue (txInfoMint info) of
    [(cs, tn, q)] -> q == amt
    _             -> traceError "unexpected mint"



--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE mkPolicyUntyped #-}
mkPolicyUntyped :: BuiltinData -> BuiltinData -> ()
mkPolicyUntyped r c =
    let red = unsafeFromBuiltinData @RebaseAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkPolicy red ctx then () else error ()


policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| mkPolicyUntyped ||])


--------------------------------------------------------------------------------
-- Currency Symbol (V2 does NOT export scriptCurrencySymbol)
--------------------------------------------------------------------------------

{-# INLINABLE scriptCurrencySymbol #-}
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol mp =
    let mph = scriptCurrencySymbol mp
    in case mph of MintingPolicyHash h -> CurrencySymbol h

currencySymbol :: CurrencySymbol
currencySymbol = PlutusV2.scriptCurrencySymbol policy


--------------------------------------------------------------------------------
-- Hash + Bech32 Address
--------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.MintingPolicy -> PlutusV2.ScriptHash
plutusValidatorHash pol =
    let bytes = Serialise.serialise pol
        short = SBS.toShort (LBS.toStrict bytes)
        builtin = Builtins.toBuiltin short
    in PlutusV2.ScriptHash builtin


toBech32PolicyAddress :: C.NetworkId -> MintingPolicy -> String
toBech32PolicyAddress network pol =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise pol
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

writePolicy :: FilePath -> MintingPolicy -> IO ()
writePolicy path pol = do
    LBS.writeFile path (Serialise.serialise pol)
    putStrLn $ "Policy written to: " <> path


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writePolicy "rebase-policy.plutus" policy

    let bech32 = toBech32PolicyAddress network policy

    putStrLn "\n--- Rebase Token Policy ---"
    putStrLn $ "CurrencySymbol: " <> P.show currencySymbol
    putStrLn $ "Bech32 Script Address: " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Rebase token policy generated successfully."

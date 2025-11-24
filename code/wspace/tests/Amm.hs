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
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
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


------------------------------------------------------------------------------------------
-- Datum / Redeemer
------------------------------------------------------------------------------------------

data PoolDatum = PoolDatum
    { pdResA       :: Integer         -- reserve A
    , pdResB       :: Integer         -- reserve B
    , pdFeeBps     :: Integer         -- fee in basis points (e.g. 30 = 0.30%)
    , pdLPCurrency :: CurrencySymbol  -- LP token policy id
    , pdPoolNFT    :: CurrencySymbol  -- pool unique NFT policy id
    , pdTokenA     :: CurrencySymbol
    , pdTokenB     :: CurrencySymbol
    , pdNameA      :: TokenName
    , pdNameB      :: TokenName
    , pdLPName     :: TokenName
    }
PlutusTx.unstableMakeIsData ''PoolDatum


data AMMAction
    = SwapAForB
    | SwapBForA
    | AddLiquidity
    | RemoveLiquidity
PlutusTx.unstableMakeIsData ''AMMAction


------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------

{-# INLINABLE getPoolInput #-}
getPoolInput :: ScriptContext -> TxOut
getPoolInput ctx =
    case findOwnInput ctx of
      Nothing -> traceError "Pool input missing"
      Just i  -> txInInfoResolved i

{-# INLINABLE getPoolOutput #-}
getPoolOutput :: ScriptContext -> TxOut
getPoolOutput ctx =
    case getContinuingOutputs ctx of
      [o] -> o
      _   -> traceError "Expected exactly 1 pool output"


{-# INLINABLE containsNFT #-}
containsNFT :: Value -> CurrencySymbol -> Bool
containsNFT v cs = valueOf v cs (TokenName emptyByteString) == 1


{-# INLINABLE applyFee #-}
applyFee :: Integer -> Integer -> Integer
applyFee amount feeBps =
    let fee = (amount * feeBps) `divide` 10000
    in amount - fee

{-# INLINABLE isqrt #-}
isqrt :: Integer -> Integer
isqrt n = isqrtIter n
  where
    isqrtIter x =
        let y = (x + n `divide` x) `divide` 2
        in if y >= x then x else isqrtIter y

------------------------------------------------------------------------------------------
-- AMM Core: Constant-product checks
------------------------------------------------------------------------------------------

{-# INLINABLE swapInvariant #-}
swapInvariant :: Integer -> Integer -> Integer -> Integer -> Bool
swapInvariant oldA oldB newA newB =
    let oldK = oldA * oldB
        newK = newA * newB
    in traceIfFalse "k decreased" (newK >= oldK)


------------------------------------------------------------------------------------------
-- LP Minting proportional share rules
------------------------------------------------------------------------------------------

{-# INLINABLE expectedLP #-}
expectedLP :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
expectedLP oldA oldB newA newB existingLP =
    if existingLP == 0
       then isqrt (newA * newB)   -- first LP provider mints sqrt(k)
       else
         let shareA = (newA - oldA) * existingLP `divide` oldA
             shareB = (newB - oldB) * existingLP `divide` oldB
         in min shareA shareB



------------------------------------------------------------------------------------------
-- MAIN VALIDATOR
------------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: PoolDatum -> AMMAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    let info      = scriptContextTxInfo ctx
        inTx      = getPoolInput ctx
        outTx     = getPoolOutput ctx
        inVal     = txOutValue inTx
        outVal    = txOutValue outTx

        oldA      = valueOf inVal  (pdTokenA dat) (pdNameA dat)
        oldB      = valueOf inVal  (pdTokenB dat) (pdNameB dat)
        newA      = valueOf outVal (pdTokenA dat) (pdNameA dat)
        newB      = valueOf outVal (pdTokenB dat) (pdNameB dat)

        lpMint    = valueOf (txInfoMint info) (pdLPCurrency dat) (pdLPName dat)
        nftOK     = containsNFT inVal (pdPoolNFT dat)
                     && containsNFT outVal (pdPoolNFT dat)

    in traceIfFalse "Pool NFT missing" nftOK &&
       case action of

         ----------------------------------------------------
         --  SWAP A → B
         ----------------------------------------------------
         SwapAForB ->
            let inputA  = newA - oldA          -- how much A user gave
                swapIn  = applyFee (negate inputA) (pdFeeBps dat)
                expectedB = oldB - (oldA * oldB) `divide` (oldA + swapIn)
                actualOutput = oldB - newB
            in traceIfFalse "wrong LP mint" (lpMint == 0) &&
               traceIfFalse "wrong B output" (actualOutput == expectedB) &&
               swapInvariant oldA oldB newA newB

         ----------------------------------------------------
         --  SWAP B → A
         ----------------------------------------------------
         SwapBForA ->
            let inputB  = newB - oldB
                swapIn  = applyFee (negate inputB) (pdFeeBps dat)
                expectedA = oldA - (oldA * oldB) `divide` (oldB + swapIn)
                actualOutput = oldA - newA
            in traceIfFalse "wrong LP mint" (lpMint == 0) &&
               traceIfFalse "wrong A output" (actualOutput == expectedA) &&
               swapInvariant oldA oldB newA newB

         ----------------------------------------------------
         -- ADD LIQUIDITY
         ----------------------------------------------------
         AddLiquidity ->
            let oldLP  = valueOf inVal  (pdLPCurrency dat) (pdLPName dat)
                newLP  = valueOf outVal (pdLPCurrency dat) (pdLPName dat)
                expected = expectedLP oldA oldB newA newB oldLP
            in traceIfFalse "wrong LP minted" (newLP - oldLP == expected)

         ----------------------------------------------------
         -- REMOVE LIQUIDITY
         ----------------------------------------------------
         RemoveLiquidity ->
            let oldLP  = valueOf inVal  (pdLPCurrency dat) (pdLPName dat)
                newLP  = valueOf outVal (pdLPCurrency dat) (pdLPName dat)
                burned = oldLP - newLP
                shareA = burned * oldA `divide` oldLP
                shareB = burned * oldB `divide` oldLP
            in traceIfFalse "LP not burned correctly" (lpMint <= 0) &&
               traceIfFalse "wrong reserves" (newA == oldA - shareA) &&
               traceIfFalse "wrong reserves" (newB == oldB - shareB)


------------------------------------------------------------------------------------------
-- Untyped wrapper
------------------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @PoolDatum d
        red = unsafeFromBuiltinData @AMMAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()


validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])


------------------------------------------------------------------------------------------
-- Script Hash + Address (exactly like your escrow)
------------------------------------------------------------------------------------------

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



------------------------------------------------------------------------------------------
-- File writer
------------------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path


------------------------------------------------------------------------------------------
-- MAIN
------------------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "amm-validator.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Constant Product AMM Validator ---"
    putStrLn $ "Validator Hash:       " <> P.show vh
    putStrLn $ "Plutus Script Address:" <> P.show onchain
    putStrLn $ "Bech32 Address:       " <> bech32
    putStrLn "---------------------------------------"
    putStrLn "AMM validator generated successfully."

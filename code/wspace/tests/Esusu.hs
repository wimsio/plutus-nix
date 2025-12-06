{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, drop, putStrLn, (<>))
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
import qualified Data.ByteString.Base16 as B16

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

-------------------------------------------------------------------------------
-- DATUM & REDEEMER
-------------------------------------------------------------------------------

data CircleDatum = CircleDatum
    { cdParticipants :: [PubKeyHash]  -- list of all N people
    , cdAmount       :: Integer       -- fixed deposit amount
    , cdOrder        :: [PubKeyHash]  -- predetermined payout order
    , cdRound        :: Integer       -- current payout round index
    }
PlutusTx.unstableMakeIsData ''CircleDatum

data CircleAction = Deposit | Payout
PlutusTx.unstableMakeIsData ''CircleAction

-------------------------------------------------------------------------------
-- HELPERS
-------------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx =
    txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE getInputAda #-}
getInputAda :: TxOut -> Integer
getInputAda o =
    valueOf (txOutValue o) adaSymbol adaToken

{-# INLINABLE getContinuing #-}
getContinuing :: ScriptContext -> TxOut
getContinuing ctx =
    case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected one continuing output"

{-# INLINABLE indexList #-}
indexList :: [a] -> Integer -> Maybe a
indexList xs idx = go xs idx
  where
    go [] _ = Nothing
    go (y:ys) i =
        if i == 0
            then Just y
            else go ys (i - 1)

{-# INLINABLE currentBeneficiary #-}
currentBeneficiary :: CircleDatum -> PubKeyHash
currentBeneficiary dat =
    case indexList (cdOrder dat) (cdRound dat) of
        Just p  -> p
        Nothing -> traceError "invalid round index"


-------------------------------------------------------------------------------
-- VALIDATOR
-------------------------------------------------------------------------------

{-# INLINABLE mkCircleValidator #-}
mkCircleValidator :: CircleDatum -> CircleAction -> ScriptContext -> Bool
mkCircleValidator dat action ctx =
    case action of
        -----------------------------------------------------------------------
        -- 1. DEPOSIT: everyone deposits fixed amount
        -----------------------------------------------------------------------
        Deposit ->
            traceIfFalse "deposit must be signed by participant"
                signedAnyParticipant
            &&
            traceIfFalse "must increase contract balance"
                depositCorrect

        -----------------------------------------------------------------------
        -- 2. PAYOUT: Only scheduled beneficiary receives funds
        -----------------------------------------------------------------------
        Payout ->
            traceIfFalse "wrong beneficiary" correctBeneficiary
            &&
            traceIfFalse "payout must reduce contract balance"
                payoutCorrect
            &&
            traceIfFalse "round must advance"
                roundIncremented

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxOut
    input = case findOwnInput ctx of
        Just i  -> txInInfoResolved i
        Nothing -> traceError "no input"

    inAda  = getInputAda input
    out    = getContinuing ctx
    outAda = getInputAda out

    signedAnyParticipant =
        any (\pkh -> signedBy pkh ctx) (cdParticipants dat)

    depositCorrect =
        outAda == inAda + cdAmount dat


    correctBeneficiary = signedBy (currentBeneficiary dat) ctx


    payoutCorrect =
        outAda == inAda - (cdAmount dat * P.fromInteger (P.toInteger (length (cdParticipants dat))))

    roundIncremented =
        case txOutDatum out of
            OutputDatum (Datum d) ->
                case PlutusTx.fromBuiltinData d of
                    Just newDat ->
                        cdRound newDat == (cdRound dat + 1)
                    _ -> traceError "invalid updated datum"
            _ -> traceError "round must update via datum"

-------------------------------------------------------------------------------
-- UNTYPED WRAPPER
-------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    if mkCircleValidator (unsafeFromBuiltinData d)
                         (unsafeFromBuiltinData r)
                         (unsafeFromBuiltinData c)
    then ()
    else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

-------------------------------------------------------------------------------
-- ADDRESS & CBOR GENERATION
-------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised
        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)
        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr = C.makeShelleyAddressInEra network (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

-------------------------------------------------------------------------------
-- FILE WRITING (PLUTUS + CBOR)
-------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

writeCBOR :: FilePath -> Validator -> IO ()
writeCBOR path val = do
    let bytes = LBS.toStrict (Serialise.serialise val)
        hex   = B16.encode bytes
    BS.writeFile path hex
    putStrLn $ "CBOR hex written to: " <> path


-------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    -- Write .plutus and .cbor hex
    writeValidator "circle.plutus" validator
    writeCBOR "circle.cbor" validator

    let vh     = plutusValidatorHash validator
        addr   = plutusScriptAddress
        bech32 = toBech32ScriptAddress network validator

    putStrLn "\n--- On-chain Savings Circle Info ---"
    putStrLn $ "Validator Hash: " <> P.show vh
    putStrLn $ "On-chain Script Address: " <> P.show addr
    putStrLn $ "Bech32 Script Address: " <> bech32
    putStrLn "--------------------------------------"

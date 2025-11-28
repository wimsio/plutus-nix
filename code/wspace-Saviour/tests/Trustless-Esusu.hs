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

-- Fixed-size Esusu (10 Participants)
data EsusuDatum = EsusuDatum
    { edParticipants :: [PubKeyHash]   -- exactly 10 PKHs
    , edRoundIndex   :: Integer         -- 0 .. 9
    , edAmount       :: Integer         -- required contribution
    , edPaid         :: [PubKeyHash]    -- who has already paid this round
    }
PlutusTx.unstableMakeIsData ''EsusuDatum

data EsusuAction
    = Contribute
    | Payout
PlutusTx.unstableMakeIsData ''EsusuAction

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE contains #-}
contains :: Eq a => a -> [a] -> Bool
contains x = any (== x)

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: EsusuDatum -> EsusuAction -> ScriptContext -> Bool
mkValidator datum action ctx =
    case action of

        --------------------------------------------------------------------
        -- CONTRIBUTION
        --------------------------------------------------------------------
        Contribute ->
            traceIfFalse "sender not in participants"
                (signedBy member ctx) &&

            traceIfFalse "already contributed this round"
                (not (contains member (edPaid datum))) &&

            traceIfFalse "must contribute exact amount"
                (inputValuePaid == edAmount datum)

            where
                info        = scriptContextTxInfo ctx
                member      = case findOwnInput ctx of
                                Just inp -> case txInfoSignatories info of
                                                (s:_) -> s
                                                _     -> traceError "no sig"
                                Nothing  -> traceError "no input"

                -- Amount ADA sent to script this round
                inputValuePaid =
                    let vIn  = valuePaidTo info (ownHash ctx)
                    in  valueOf vIn adaSymbol adaToken

        --------------------------------------------------------------------
        -- PAYOUT
        --------------------------------------------------------------------
        Payout ->
            traceIfFalse "round not complete (not all contributed)"
                (length (edPaid datum) == 10) &&

            traceIfFalse "must pay correct recipient"
                (valuePaidTo info recipientPKH >= totalPot) &&

            traceIfFalse "output datum must increment round"
                isRoundIncremented

            where
                info         = scriptContextTxInfo ctx
                totalPot     = 10 * (edAmount datum)
                recipientPKH = edParticipants datum !! P.fromIntegral (edRoundIndex datum)

                -- New datum should have round +1 mod 10 and empty edPaid
                isRoundIncremented =
                    case getContinuingOutputs ctx of
                        [o] ->
                            case txOutDatum o of
                                OutputDatum (Datum d) ->
                                    case unsafeFromBuiltinData @EsusuDatum d of
                                        newD ->
                                            edRoundIndex newD ==
                                                ((edRoundIndex datum + 1) `mod` 10)
                                                &&
                                            edPaid newD == []
                                _ -> False
                        _ -> False

    where
        ownHash :: ScriptContext -> ValidatorHash
        ownHash = ownHash'

{-# INLINABLE ownHash' #-}
ownHash' :: ScriptContext -> ValidatorHash
ownHash' ctx =
    case findOwnInput ctx of
        Just i  -> case txOutAddress (txInInfoResolved i) of
                     Address (ScriptCredential vh) _ -> vh
                     _ -> traceError "not script input"
        Nothing -> traceError "no script input"

------------------------------------------------------------------------
-- Untyped Wrapper
------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let datum = unsafeFromBuiltinData @EsusuDatum d
        act   = unsafeFromBuiltinData @EsusuAction r
        ctx   = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator datum act ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------
-- Hashing + Addresses + CBOR Hex
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
-- File Writing
------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------
-- MAIN (Generates Plutus File + CBOR)
------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)
    writeValidator "esusu-circle.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Trustless Esusu Circle Validator Info ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------------------"
    putStrLn "Esusu circle validator generated successfully."

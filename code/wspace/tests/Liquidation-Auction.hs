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

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

-- Cardano API
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

------------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------------

data AuctionMode = Surplus | Deficit
PlutusTx.unstableMakeIsData ''AuctionMode

data AuctionDatum = AuctionDatum
    { adLot       :: Value
    , adQuote     :: Value
    , adStart     :: POSIXTime
    , adEnd       :: POSIXTime
    , adMinInc    :: Integer
    , adTopBid    :: Integer
    , adTopBidder :: PubKeyHash
    , adMode      :: AuctionMode
    , adKicker    :: PubKeyHash
    }
PlutusTx.unstableMakeIsData ''AuctionDatum

data AuctionAction = Bid Integer PubKeyHash | Settle | Cancel
PlutusTx.unstableMakeIsData ''AuctionAction

------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------

{-# INLINABLE withinTime #-}
withinTime :: POSIXTime -> POSIXTime -> POSIXTime -> Bool
withinTime start end now = Interval.contains (Interval.interval start end) (Interval.singleton now)

{-# INLINABLE signedBy #-}
signedBy :: PubKeyHash -> ScriptContext -> Bool
signedBy pkh ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE bidValid #-}
bidValid :: Integer -> Integer -> Integer -> Bool
bidValid newBid topBid minInc = newBid >= topBid + minInc

------------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: AuctionDatum -> AuctionAction -> ScriptContext -> Bool
mkValidator dat action ctx =
    case action of
        Bid amount bidder ->
            traceIfFalse "auction not active" (withinTime (adStart dat) (adEnd dat) now) &&
            traceIfFalse "invalid increment" (bidValid amount (adTopBid dat) (adMinInc dat))
        Settle ->
            traceIfFalse "auction not ended" (now > adEnd dat)
        Cancel ->
            traceIfFalse "only kicker can cancel" (signedBy (adKicker dat) ctx)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    now :: POSIXTime
    now = case ivTo (txInfoValidRange info) of
            UpperBound (Finite t) _ -> t
            _                        -> traceError "invalid time range"

------------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------------

{-# INLINABLE mkValidatorUntyped #-}
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped d r c =
    let dat = unsafeFromBuiltinData @AuctionDatum d
        red = unsafeFromBuiltinData @AuctionAction r
        ctx = unsafeFromBuiltinData @ScriptContext c
    in if mkValidator dat red ctx then () else error ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidatorUntyped ||])

------------------------------------------------------------------------------
-- On-chain hash and addresses
------------------------------------------------------------------------------

plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash validator =
    let bytes    = Serialise.serialise validator
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

plutusScriptAddress :: Address
plutusScriptAddress = Address (ScriptCredential (plutusValidatorHash validator)) Nothing

------------------------------------------------------------------------------
-- Off-chain Bech32
------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
-- File writing
------------------------------------------------------------------------------

writeValidator :: FilePath -> Validator -> IO ()
writeValidator path val = do
    LBS.writeFile path (Serialise.serialise val)
    putStrLn $ "Validator written to: " <> path

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
    let network = C.Testnet (C.NetworkMagic 1)

    writeValidator "auction.plutus" validator

    let vh      = plutusValidatorHash validator
        onchain = plutusScriptAddress
        bech32  = toBech32ScriptAddress network validator

    putStrLn "\n--- Liquidation Auction Validator ---"
    putStrLn $ "Validator Hash (Plutus): " <> P.show vh
    putStrLn $ "Plutus Script Address:    " <> P.show onchain
    putStrLn $ "Bech32 Script Address:    " <> bech32
    putStrLn "---------------------------------"
    putStrLn "Auction validator generated successfully."

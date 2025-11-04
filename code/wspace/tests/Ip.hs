{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Prelude (IO, FilePath, putStrLn, String, (++), show)
import qualified Prelude as H
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- IP Datum and Redeemer
--------------------------------------------------------------------------------

data IPDatum = IPDatum
    { ipOwner     :: PubKeyHash
    , ipTitle     :: BuiltinByteString
    , ipContentId :: BuiltinByteString  -- hash of the intellectual property
    , ipLicensed  :: Bool
    }
    deriving (H.Show, Generic)

-- Only need IsData for Datum usage
PlutusTx.unstableMakeIsData ''IPDatum

data IPRedeemer
    = Register               -- new IP registration
    | Transfer PubKeyHash     -- transfer to a new owner
    deriving (H.Show, Generic)

PlutusTx.unstableMakeIsData ''IPRedeemer

--------------------------------------------------------------------------------
-- Validation Logic
--------------------------------------------------------------------------------

{-# INLINABLE mkIPValidator #-}
mkIPValidator :: IPDatum -> IPRedeemer -> ScriptContext -> Bool
mkIPValidator datum red ctx =
    case red of
        Register       -> validateRegister datum ctx
        Transfer newPk -> validateTransfer datum newPk ctx

{-# INLINABLE validateRegister #-}
validateRegister :: IPDatum -> ScriptContext -> Bool
validateRegister dat ctx =
    traceIfFalse "registration not signed by owner" signedByOwner
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    signedByOwner :: Bool
    signedByOwner = txSignedBy info (ipOwner dat)

{-# INLINABLE validateTransfer #-}
validateTransfer :: IPDatum -> PubKeyHash -> ScriptContext -> Bool
validateTransfer dat newOwner ctx =
    traceIfFalse "transfer not signed by current owner" signedByOldOwner &&
    traceIfFalse "no continuing output with new owner" hasValidContinuing
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByOldOwner :: Bool
    signedByOldOwner = txSignedBy info (ipOwner dat)

    continuing :: [TxOut]
    continuing = getContinuingOutputs ctx

    hasValidContinuing :: Bool
    hasValidContinuing =
        case continuing of
            [o] -> case txOutDatum o of
                OutputDatum (Datum d) ->
                    case PlutusTx.fromBuiltinData d of
                        Just (IPDatum newOwner' title' content' licensed') ->
                            newOwner' == newOwner &&
                            title' == ipTitle dat &&
                            content' == ipContentId dat &&
                            licensed' == ipLicensed dat
                        _ -> False
                _ -> False
            _ -> False

--------------------------------------------------------------------------------
-- Compile and Wrap
--------------------------------------------------------------------------------

{-# INLINABLE mkUntypedValidator #-}
mkUntypedValidator ::
    (IPDatum -> IPRedeemer -> ScriptContext -> Bool)
    -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntypedValidator f d r c =
    check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript
    $$(PlutusTx.compile [|| mkUntypedValidator mkIPValidator ||])

--------------------------------------------------------------------------------
-- Write validator file
--------------------------------------------------------------------------------

writeValidator :: H.FilePath -> Validator -> IO ()
writeValidator file val = do
    let script = unValidatorScript val
        bytes  = serialise script
    LBS.writeFile file bytes
    H.putStrLn $ "Wrote validator to: " H.++ file

main :: IO ()
main = do
    let file = "ip-smartcontract.plutus"
    writeValidator file validator
    H.putStrLn "IP Smart Contract (no makeLift, modern PlutusTx) built successfully!"


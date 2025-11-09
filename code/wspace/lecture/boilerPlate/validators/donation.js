export const donationFile = `
   
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin PlutusTx.Plugin #-}

module Donation where

import PlutusTx (compile, unsafeFromBuiltinData, CompiledCode, unstableMakeIsData)
import PlutusTx.Prelude hiding (($), (<>))
import Plutus.V2.Ledger.Contexts 
    ( txSignedBy
    , ScriptContext(..)
    , TxInfo(..)
    , TxOut(..)
    , TxInInfo(..)
    , scriptContextPurpose
    , txInInfoResolved
    , ScriptPurpose(..)
    , TxOutRef(..)
    , findDatum
    )
import Plutus.V2.Ledger.Api 
    ( BuiltinData
    , Validator
    , mkValidatorScript
    , POSIXTime
    , PubKeyHash
    , ValidatorHash
    , Address(..)
    , Credential(..)
    , Datum(..)
    , DatumHash
    , OutputDatum(..)
    )
import Plutus.V1.Ledger.Interval (contains, to, from)
import Plutus.V1.Ledger.Value (Value, flattenValue, adaSymbol, adaToken)

import Prelude (IO, Show, print, putStrLn, FilePath, ($), (<>))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Codec.Serialise (serialise)
import Cardano.Api
    ( writeFileTextEnvelope
    , displayError
    , PlutusScriptV2
    )
import Cardano.Api.Shelley (PlutusScript (..))
import qualified Data.List as L

-- ===========================
-- | Donation parameters     |
-- ===========================

{-# INLINABLE minDonation #-}
minDonation :: Integer
minDonation = 1000000  -- 1 ADA in lovelace

{-# INLINABLE getLovelace #-}
getLovelace :: Value -> Integer
getLovelace v =
    foldl (acc (cs, tn, amt) -> if cs == adaSymbol && tn == adaToken then acc + amt else acc) 0 (flattenValue v)

{-# INLINABLE addrValidatorHash #-}
addrValidatorHash :: Address -> Maybe ValidatorHash
addrValidatorHash (Address (ScriptCredential vh) _) = Just vh
addrValidatorHash _                                 = Nothing

{-# INLINABLE ownValidatorHash #-}
ownValidatorHash :: ScriptContext -> ValidatorHash
ownValidatorHash ctx = 
    case scriptContextPurpose ctx of
        Spending txOutRef ->
            let info = scriptContextTxInfo ctx
                mInput = L.find (i -> txInInfoOutRef i == txOutRef) (txInfoInputs info)
            in case mInput of
                Just txIn -> case addrValidatorHash (txOutAddress $ txInInfoResolved txIn) of
                               Just h  -> h
                               Nothing -> traceError "Invalid script input"
                Nothing -> traceError "Input not found"
        _ -> traceError "Not spending from script"

{-# INLINABLE donationAmount #-}
donationAmount :: ScriptContext -> Integer
donationAmount ctx =
    let
        info :: TxInfo
        info = scriptContextTxInfo ctx
        ownHash = ownValidatorHash ctx

        outputsToScript :: [Value]
        outputsToScript = [ txOutValue o | o <- txInfoOutputs info
                                         , Just h <- [addrValidatorHash $ txOutAddress o]
                                         , h == ownHash ]
    in sum (map getLovelace outputsToScript)

-- ===========================
-- | Custom On-chain Types   |
-- ===========================

data CampaignDatum = CampaignDatum
    { beneficiary  :: PubKeyHash
    , deadline     :: POSIXTime
    , goal         :: Integer
    , totalDonated :: Integer
    } deriving (Show, Generic)

data CampaignActions = Donate | Withdraw | Refund
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''CampaignDatum
PlutusTx.unstableMakeIsData ''CampaignActions

-- ===========================
-- | On-chain equality       |
-- ===========================

{-# INLINABLE eqCampaignDatum #-}
eqCampaignDatum :: CampaignDatum -> CampaignDatum -> Bool
eqCampaignDatum d1 d2 =
    beneficiary d1 == beneficiary d2 &&
    deadline d1    == deadline d2 &&
    goal d1        == goal d2 &&
    totalDonated d1 == totalDonated d2

-- ===========================
-- | Validator Logic         |
-- ===========================

{-# INLINABLE mkCampaignValidator #-}
mkCampaignValidator :: CampaignDatum -> CampaignActions -> ScriptContext -> Bool
mkCampaignValidator dat red ctx =
    case red of
        Donate   -> traceIfFalse "Too late to donate or amount too low" canDonate &&
                    traceIfFalse "Output datum not updated correctly" datumUpdated
        Withdraw -> traceIfFalse "Not allowed to withdraw" canWithdraw
        Refund   -> traceIfFalse "Cannot refund" canRefund
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx
    ownHash = ownValidatorHash ctx

    -- Donation logic
    canDonate :: Bool
    canDonate = to (deadline dat) \`contains\` txInfoValidRange info &&
                donationAmount ctx >= minDonation

    updatedDatum :: CampaignDatum
    updatedDatum = dat { totalDonated = totalDonated dat + donationAmount ctx }

    datumUpdated :: Bool
    datumUpdated =
        let outputs = [ o | o <- txInfoOutputs info
                          , Just h <- [addrValidatorHash $ txOutAddress o]
                          , h == ownHash ]
        in case outputs of
            [o] -> case txOutDatum o of
                     OutputDatumHash dh -> case findDatum dh info of
                                              Just (Datum d') -> eqCampaignDatum (unsafeFromBuiltinData d') updatedDatum
                                              Nothing         -> False
                     _ -> False
            _   -> False

    -- Withdraw logic
    canWithdraw :: Bool
    canWithdraw = txSignedBy info (beneficiary dat) &&
                  from (deadline dat) \`contains\` txInfoValidRange info &&
                  totalDonated dat >= goal dat

    -- Refund logic
    canRefund :: Bool
    canRefund = from (deadline dat) \`contains\` txInfoValidRange info &&
                totalDonated dat < goal dat

{-# INLINABLE wrap #-}
wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrap d r c =
    let dat = unsafeFromBuiltinData d
        red = unsafeFromBuiltinData r
        ctx = unsafeFromBuiltinData c
    in if mkCampaignValidator dat red ctx then () else error ()

compiledCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
compiledCode = $$(compile [|| wrap ||])

validator :: Validator
validator = mkValidatorScript compiledCode

-- ===========================
-- | Write Validator to File |
-- ===========================

writeValidator :: FilePath -> Validator -> IO ()
writeValidator file validator = do
    let scriptSerialised = serialise validator
        scriptShort = SBS.toShort . LBS.toStrict $ scriptSerialised
        script :: PlutusScript PlutusScriptV2
        script = PlutusScriptSerialised scriptShort
    result <- writeFileTextEnvelope file Nothing script
    case result of
        Left err -> print (displayError err)
        Right () -> putStrLn ("Wrote validator to: " <> file)

-- ===========================
-- | Main                    |
-- ===========================

main :: IO ()
main = do
    putStrLn "Validator compiled successfully!"
    writeValidator "./assets/campaign-validator.plutus" validator
    `


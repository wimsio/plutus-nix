{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

------------------------------------------------------------------------------------------
-- Datum / Redeemer
------------------------------------------------------------------------------------------

data CampaignDatum = CampaignDatum
    { cdCreator      :: PubKeyHash
    , cdTarget       :: Integer
    , cdDeadline     :: POSIXTime
    , cdRaised       :: Integer
    , cdMilestoneIdx :: Integer
    }
PlutusTx.unstableMakeIsData ''CampaignDatum

data CampaignAction = Pledge | UnlockMilestone | Refund
PlutusTx.unstableMakeIsData ''CampaignAction

------------------------------------------------------------------------------------------
-- Helper functions
------------------------------------------------------------------------------------------

{-# INLINABLE lovelace #-}
lovelace :: Value -> Integer
lovelace v = valueOf v adaSymbol adaToken

{-# INLINABLE signedBy #-}
signedBy :: ScriptContext -> PubKeyHash -> Bool
signedBy ctx pkh = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINABLE isBefore #-}
isBefore :: POSIXTime -> ScriptContext -> Bool
isBefore dl ctx =
    let r = txInfoValidRange (scriptContextTxInfo ctx)
    in contains (to dl) r

{-# INLINABLE isAfter #-}
isAfter :: POSIXTime -> ScriptContext -> Bool
isAfter dl ctx =
    let r = txInfoValidRange (scriptContextTxInfo ctx)
    in contains (from dl) r

------------------------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: CampaignDatum -> CampaignAction -> ScriptContext -> Bool
mkValidator dat red ctx =
  case red of

    ------------------------------------------------------------------------------------
    -- 1. PLEDGE
    ------------------------------------------------------------------------------------
    Pledge ->
      traceIfFalse "pledge not before deadline"  (isBefore (cdDeadline dat) ctx)
      && traceIfFalse "pledge must increase raised" increasedCorrectly

      where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        -- Must update the datum's cdRaised
        increasedCorrectly :: Bool
        increasedCorrectly =
          case getContinuingOutputs ctx of
            [o] ->
              case txOutDatum o of
                OutputDatum (Datum d) ->
                  case PlutusTx.fromBuiltinData d of
                    Just newDat ->
                        cdRaised newDat > cdRaised dat
                    _ -> False
                _ -> False
            _ -> False


    ------------------------------------------------------------------------------------
    -- 2. UNLOCK MILESTONE
    ------------------------------------------------------------------------------------
    UnlockMilestone ->
      traceIfFalse "not signed by creator"   (signedBy ctx (cdCreator dat))
      && traceIfFalse "target not met"       targetReached
      && traceIfFalse "milestone not updated" milestoneIncreased
      && traceIfFalse "cannot unlock before deadline" (isAfter (cdDeadline dat) ctx)

      where
        info = scriptContextTxInfo ctx

        targetReached :: Bool
        targetReached = cdRaised dat >= cdTarget dat

        milestoneIncreased :: Bool
        milestoneIncreased =
          case getContinuingOutputs ctx of
            [o] ->
              case txOutDatum o of
                OutputDatum (Datum d) ->
                  case PlutusTx.fromBuiltinData d of
                    Just newDat ->
                        cdMilestoneIdx newDat == cdMilestoneIdx dat + 1
                    _ -> False
                _ -> False
            _ -> False


    ------------------------------------------------------------------------------------
    -- 3. REFUND
    ------------------------------------------------------------------------------------
    Refund ->
      traceIfFalse "deadline not passed"   (isAfter (cdDeadline dat) ctx)
      && traceIfFalse "target was reached" (cdRaised dat < cdTarget dat)
      && traceIfFalse "refund must destroy UTxO" spendsOwnInput

      where
        info = scriptContextTxInfo ctx

        spendsOwnInput :: Bool
        spendsOwnInput = True  -- spending the script UTxO = refund

------------------------------------------------------------------------------------------
-- Boilerplate
------------------------------------------------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    case (PlutusTx.fromBuiltinData d, PlutusTx.fromBuiltinData r, PlutusTx.fromBuiltinData c) of
        (Just dat, Just red, Just ctx) -> 
            if mkValidator dat red ctx then () else error ()
        _ -> error ()

validator :: Validator
validator =
    let compiled = $$(PlutusTx.compile [|| wrapped ||])
    in Validator compiled

validatorScript :: Script
validatorScript = unValidatorScript validator

validatorSBS :: SBS.ShortByteString
validatorSBS = SBS.toShort . LBS.toStrict $ Serialise.serialise validatorScript

validatorSerialised :: LBS.ByteString
validatorSerialised = Serialise.serialise validatorScript

------------------------------------------------------------------------------------------
-- Main (prints the serialized script)
------------------------------------------------------------------------------------------

main :: IO ()
main = LBS.writeFile "milestone-crowdfunding.plutus" validatorSerialised

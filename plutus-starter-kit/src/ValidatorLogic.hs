{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module ValidatorLogic
  ( CoxyDatum
  , CoxyRedeemer
  , mkValidator
  ) where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import qualified PlutusTx as PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS

------------------------------------------------------------------------
-- Datum and Redeemer
------------------------------------------------------------------------

data CoxyDatum = CoxyDatum
    { cdSeller :: PubKeyHash
    , cdBuyer :: PubKeyHash
    , cdAmount :: Integer
    , cdDeadline :: POSIXTime
    , cdCurrency :: CurrencySymbol
    , cdToken :: TokenName
    }
PlutusTx.unstableMakeIsData ''CoxyDatum

data CoxyRedeemer = PaySeller | RefundBuyer
PlutusTx.unstableMakeIsData ''CoxyRedeemer

------------------------------------------------------------------------
-- Validator Logic
------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: CoxyDatum -> CoxyRedeemer -> ScriptContext -> Bool
mkValidator dat red ctx =
  case red of
    PaySeller ->
      traceIfFalse "signedByBuyer" (constraint_signedByBuyer dat ctx)
      && traceIfFalse "signedBySeller" (constraint_signedBySeller dat ctx)
      && traceIfFalse "beforeDeadline" (constraint_beforeDeadline dat ctx)
      && traceIfFalse "sellerPaid" (constraint_sellerPaid dat ctx)
      && traceIfFalse "scriptHasNFT" (constraint_scriptHasNFT dat ctx)
    RefundBuyer ->
      traceIfFalse "signedBySeller" (constraint_signedBySeller dat ctx)
      && traceIfFalse "afterDeadline" (constraint_afterDeadline dat ctx)
      && traceIfFalse "buyerRefunded" (constraint_buyerRefunded dat ctx)
      && traceIfFalse "scriptHasNFT" (constraint_scriptHasNFT dat ctx)
  where
    constraint_signedByBuyer :: CoxyDatum -> ScriptContext -> Bool
    constraint_signedByBuyer _ _ = True

    constraint_signedBySeller :: CoxyDatum -> ScriptContext -> Bool
    constraint_signedBySeller _ _ = True

    constraint_beforeDeadline :: CoxyDatum -> ScriptContext -> Bool
    constraint_beforeDeadline _ _ = True

    constraint_sellerPaid :: CoxyDatum -> ScriptContext -> Bool
    constraint_sellerPaid _ _ = True

    constraint_scriptHasNFT :: CoxyDatum -> ScriptContext -> Bool
    constraint_scriptHasNFT _ _ = True

    constraint_afterDeadline :: CoxyDatum -> ScriptContext -> Bool
    constraint_afterDeadline _ _ = True

    constraint_buyerRefunded :: CoxyDatum -> ScriptContext -> Bool
    constraint_buyerRefunded _ _ = True



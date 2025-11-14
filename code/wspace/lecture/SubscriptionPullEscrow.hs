{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Subscription Pull Escrow (SPE)
-- Merchant can pull up to a limit per period from subscriber’s escrow.
-- Datum tracks window + spending; redeemers implement Charge | Cancel | TopUp | Update.
--
-- Security model (high level):
--  • Only the merchant may "Charge" funds out to their own keyhash address.
--  • Only the subscriber may "Cancel" (close the escrow and reclaim all funds).
--  • Only the subscriber may "TopUp" (add funds) or "Update" (change params).
--  • Spending is bounded by an on-chain period+limit. The window resets automatically
--    based on the transaction's lower validity bound.
--
-- Notes:
--  • This validator is single-UTxO stateful: one escrow UTxO holds all funds
--    and carries the SubDatum state. Each transition must consume and recreate
--    the contract UTxO, except Cancel which destroys it.
--  • For production, consider adding a unique NFT/thread token in the escrow
--    to prevent duplicate UTxOs and simplify discovery.

module SubscriptionPullEscrow (validator, validatorHash, script, writeUplc) where

import           GHC.Generics                 (Generic)
import           Prelude                      (IO, Show (..))
import           PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), unless)

import           Plutus.V2.Ledger.Api         as V2
import           Plutus.V2.Ledger.Contexts    as V2
import           Plutus.V1.Ledger.Value       as Value
import           Plutus.V1.Ledger.Credential     (PaymentPubKeyHash (..))
import qualified Plutus.V1.Ledger.Interval    as Interval

--------------------------------------------------------------------------------
-- On-chain types
--------------------------------------------------------------------------------

-- | Subscription datum
--   'resetAt' is the start of the current window; it slides forward in multiples of 'period'.
--   'spentInPeriod' is the total paid to the merchant within the current window.
data SubDatum = SubDatum
  { subscriber     :: PaymentPubKeyHash
  , merchant       :: PaymentPubKeyHash
  , period         :: POSIXTime -- window length in ms
  , limit          :: Integer   -- max lovelace per period
  , spentInPeriod  :: Integer   -- lovelace already charged in current window
  , resetAt        :: POSIXTime -- window anchor (inclusive)
  }
  deriving stock (Generic, Show)

PlutusTx.makeIsDataIndexed ''SubDatum
  [ ( 'SubDatum, 0 ) ]
PlutusTx.makeLift ''SubDatum

-- | Supported actions
data Action = Charge | Cancel | TopUp | Update
  deriving stock (Generic, Show)

PlutusTx.makeIsDataIndexed ''Action
  [ ('Charge, 0)
  , ('Cancel, 1)
  , ('TopUp,  2)
  , ('Update, 3)
  ]

PlutusTx.makeLift ''Action
PlutusTx.deriveEq ''Action  -- optional, if you compare Actions on-chain


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE fromLowerBound #-}
fromLowerBound :: POSIXTimeRange -> POSIXTime
fromLowerBound rng = case ivFrom rng of
  LowerBound (Finite t) _ -> t
  _                       -> traceError "SPE: open-begin range"

{-# INLINABLE valuePaidToPkh #-}
valuePaidToPkh :: TxInfo -> PaymentPubKeyHash -> Integer
valuePaidToPkh info (PaymentPubKeyHash pkh) =
  let outs = txInfoOutputs info
  in  foldr (\o acc ->
        case toPubKeyHash (txOutAddress o) of
          Just pkh' | pkh' == pkh -> acc + (lovelaceOf $ txOutValue o)
          _                       -> acc) 0 outs

{-# INLINABLE lovelaceOf #-}
lovelaceOf :: Value -> Integer
lovelaceOf = getLovelace . Ada.getLovelace . Ada.fromValue

{-# INLINABLE signedBy #-}
signedBy :: TxInfo -> PaymentPubKeyHash -> Bool
signedBy info (PaymentPubKeyHash pkh) = txSignedBy info pkh

{-# INLINABLE findOwnInOut #-}
findOwnInOut :: ScriptContext -> (TxOut, TxOut)
findOwnInOut ctx =
  let info = scriptContextTxInfo ctx
      inRef = case findOwnInput ctx of
        Nothing -> traceError "SPE: own input not found"
        Just i  -> txInInfoResolved i
      out = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "SPE: expected one continuing output"
  in (inRef, out)

{-# INLINABLE decodeDatum #-}
decodeDatum :: TxOut -> SubDatum
decodeDatum o = case txOutDatum o of
  OutputDatum (Datum d) -> case PlutusTx.fromBuiltinData d of
    Just sd -> sd
    _       -> traceError "SPE: bad inline datum"
  _ -> traceError "SPE: expected inline datum"

{-# INLINABLE encodeDatum #-}
encodeDatum :: SubDatum -> OutputDatum
encodeDatum = OutputDatum . Datum . PlutusTx.toBuiltinData

--------------------------------------------------------------------------------
-- Window reset logic
--------------------------------------------------------------------------------

-- | Slide the window so that 'now' \in [resetAt, resetAt + period).
--   If we moved by >= one period, spent resets to 0. We may need to step
--   multiple periods forward if the tx is long after the previous window.
{-# INLINABLE advanceWindow #-}
advanceWindow :: POSIXTime -> POSIXTime -> POSIXTime -> (POSIXTime, Integer)
advanceWindow now reset p =
  if now < reset then (reset, 0) -- shouldn't happen with valid ranges, but safe
  else
    let diff = now - reset
        steps = if p <= 0 then traceError "SPE: nonpositive period" else diff `divide` p
    in if steps == 0
         then (reset, 1) -- 1 means "same window"
         else (reset + p * steps, 0) -- reset spent; moved forward

--------------------------------------------------------------------------------
-- Core validator
--------------------------------------------------------------------------------

{-# INLINABLE mkValidator #-}
mkValidator :: SubDatum -> Action -> ScriptContext -> Bool
mkValidator oldDatum action ctx =
  let info = scriptContextTxInfo ctx
      (inRef, outRef) = case action of
        -- Cancel consumes the script UTxO and must NOT create a continuing output
        cancel ->
          let _ = case getContinuingOutputs ctx of
                    [] -> ()
                    _  -> traceError "SPE: cancel must not continue"
          in (case findOwnInput ctx of
               Nothing -> traceError "SPE: own input not found"
               Just i  -> (txInInfoResolved i, traceError "unreachable"))
        _ -> findOwnInOut ctx

      old@SubDatum{ subscriber = sub, merchant = mch, period = p, limit = lim, spentInPeriod = spent0, resetAt = r0 } = oldDatum
      now = fromLowerBound (txInfoValidRange info)

      -- apply time window advancement for non-cancel paths
      (r1, spentBaseFlag) = advanceWindow now r0 p
      spentBase = case spentBaseFlag of
        0 -> 0
        _ -> spent0

      -- continuing datum (if any) must use inline datum
      newDatum = if action == cancel then old else decodeDatum outRef
      SubDatum{ subscriber = sub', merchant = mch', period = p', limit = lim', spentInPeriod = spent', resetAt = r' } = newDatum

      adaIn  = lovelaceOf (txOutValue inRef)
      adaOut = if action == cancel then 0 else lovelaceOf (txOutValue outRef)
      paidToMerchant = valuePaidToPkh info mch
      remains = lim - spentBase

      guard :: Bool -> BuiltinString -> Bool
      guard b msg = if b then True else traceError msg

  in case action of

    --------------------------------------------------------------------------
    -- Merchant Charge
    --------------------------------------------------------------------------
    charge ->
      traceIfFalse "SPE: charge must be by merchant" (signedBy info mch) &&
      -- funds must be paid directly to merchant PKH
      traceIfFalse "SPE: no merchant payment" (paidToMerchant > 0) &&
      -- cannot exceed remaining allowance in the (possibly advanced) window
      traceIfFalse "SPE: over period limit" (paidToMerchant <= remains) &&
      -- must re-create the UTxO with updated spent and possibly advanced reset
      traceIfFalse "SPE: subscriber immutable" (sub' == sub) &&
      traceIfFalse "SPE: period immutable on charge" (p' == p) &&
      traceIfFalse "SPE: limit immutable on charge" (lim' == lim) &&
      traceIfFalse "SPE: reset monotone" (r' == r1) &&
      traceIfFalse "SPE: spent mismatch" (spent' == spentBase + paidToMerchant) &&
      -- value should go down by the amount paid out (ignoring fees)
      traceIfFalse "SPE: escrow decreased by paid amount" (adaIn >= adaOut + paidToMerchant)

    --------------------------------------------------------------------------
    -- Subscriber Cancel
    --------------------------------------------------------------------------
    cancel ->
      traceIfFalse "SPE: cancel must be by subscriber" (signedBy info sub) &&
      -- All ADA from the script input must be paid to the subscriber
      traceIfFalse "SPE: funds not paid to subscriber" (valuePaidToPkh info sub == adaIn)

    --------------------------------------------------------------------------
    -- Subscriber TopUp (add funds; may also advance window)
    --------------------------------------------------------------------------
    topUp ->
      traceIfFalse "SPE: topup must be by subscriber" (signedBy info sub) &&
      traceIfFalse "SPE: subscriber immutable" (sub' == sub) &&
      traceIfFalse "SPE: merchant immutable on topup" (mch' == mch) &&
      traceIfFalse "SPE: period immutable on topup" (p' == p) &&
      traceIfFalse "SPE: limit immutable on topup" (lim' == lim) &&
      traceIfFalse "SPE: reset monotone" (r' == r1) &&
      traceIfFalse "SPE: spent preserved/reset" (spent' == spentBase) &&
      traceIfFalse "SPE: escrow must increase" (adaOut > adaIn)

    --------------------------------------------------------------------------
    -- Subscriber Update (change period/limit/merchant; subscriber-only)
    --------------------------------------------------------------------------
    update ->
      traceIfFalse "SPE: update must be by subscriber" (signedBy info sub) &&
      traceIfFalse "SPE: subscriber immutable" (sub' == sub) &&
      -- allow merchant, period, limit to change
      traceIfFalse "SPE: reset monotone" (r' == r1) &&
      traceIfFalse "SPE: spent preserved/reset" (spent' == spentBase) &&
      traceIfFalse "SPE: escrow preserved" (adaOut == adaIn)

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d r c =
  check $ mkValidator (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWrapped ||])

validatorHash :: ValidatorHash
validatorHash = V2.validatorHash validator

script :: Script
script = unValidatorScript validator

-- Simple helper to write UPLC to a file when used off-chain.
writeUplc :: FilePath -> IO ()
writeUplc fp = writeFileTextEnvelope fp Nothing validator >>= \case
  Left err -> Prelude.error (show err)
  Right () -> Prelude.pure ()

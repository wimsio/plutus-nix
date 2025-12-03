{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Universal where

-- same import style as Vesting & ParameterizedVesting
import Plutus.V1.Ledger.Interval (contains, from, interval)
import Plutus.V2.Ledger.Api
  ( BuiltinData, CurrencySymbol, Datum(..), MintingPolicy,
    POSIXTime, PubKeyHash(..), Redeemer(..), ScriptContext(..),
    ScriptPurpose(..), TokenName, TxInfo(..), TxOut(..), Validator,
    mkMintingPolicyScript, mkValidatorScript, Address(..),
    Credential(..), ValidatorHash, Value, fromBuiltinData,
    toBuiltinData, unsafeFromBuiltinData )
import Plutus.V2.Ledger.Contexts
  ( scriptContextTxInfo, getContinuingOutputs, findOwnInput,
    ownHash, txInInfoResolved, txSignedBy )
import PlutusTx
  ( applyCode, compile, liftCode, makeLift ,makeIsDataIndexed)

import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool(..), Integer, traceIfFalse, traceError,
    (&&), (==), (||), (+), (<=), (>=), mempty,
    Maybe(..), filter, foldr, ($), map )
import Prelude (Show)
import qualified PlutusTx.AssocMap as Map

--------------------------------------------------------------------------------
-- Parameters & Rules
--------------------------------------------------------------------------------

-- 1) Type declarations (define each ONCE)
data Cmp
  = EQI Integer
  | GE Integer
  | LE Integer
  | BETWEEN Integer Integer

data Rule
  = RequireAnySigner [PubKeyHash]
  | ValidAfter POSIXTime
  | ValidWithin POSIXTime POSIXTime
  | PreserveValueOneContinuing
  | NoMint
  | SameScriptContinuing
  | MintAmount CurrencySymbol TokenName Cmp
  | Pass

newtype SpendingSpec = SpendingSpec { spendingRules :: [Rule] }

newtype MintingSpec  = MintingSpec  { mintingRules  :: [Rule] }

data Params = Params
  { sSpec :: SpendingSpec
  , mSpec :: MintingSpec
  , extra :: [BuiltinData]
  }

-- 2) TH instances (derive IsData and Lift exactly ONCE for each)
PlutusTx.makeIsDataIndexed ''Cmp
  [ ('EQI, 0), ('GE, 1), ('LE, 2), ('BETWEEN, 3) ]
PlutusTx.makeLift ''Cmp

PlutusTx.makeIsDataIndexed ''Rule
  [ ('RequireAnySigner, 0), ('ValidAfter, 1), ('ValidWithin, 2)
  , ('PreserveValueOneContinuing, 3), ('NoMint, 4)
  , ('SameScriptContinuing, 5), ('MintAmount, 6), ('Pass, 7)
  ]
PlutusTx.makeLift ''Rule

PlutusTx.makeIsDataIndexed ''SpendingSpec [('SpendingSpec, 0)]
PlutusTx.makeLift ''SpendingSpec

PlutusTx.makeIsDataIndexed ''MintingSpec  [('MintingSpec,  0)]
PlutusTx.makeLift ''MintingSpec

PlutusTx.makeIsDataIndexed ''Params      [('Params,      0)]
PlutusTx.makeLift ''Params

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE allChecks #-}
allChecks :: [Bool] -> Bool
allChecks = foldr (&&) True

{-# INLINABLE anyP #-}
anyP :: (a -> Bool) -> [a] -> Bool
anyP p = foldr (\x acc -> p x || acc) False

{-# INLINABLE mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> case f x of { Just y -> y : acc; _ -> acc }) []

--------------------------------------------------------------------------------
-- Datum / Redeemer Collectors
--------------------------------------------------------------------------------

{-# INLINABLE collectDatumsOf #-}
collectDatumsOf :: forall d. (PlutusTx.FromData d) => ScriptContext -> [d]
collectDatumsOf ctx =
  let info = scriptContextTxInfo ctx
  in mapMaybe (\(_, Datum b) -> fromBuiltinData b) (Map.toList $ txInfoData info)

{-# INLINABLE collectRedeemersOf #-}
collectRedeemersOf :: forall r. (PlutusTx.FromData r) => ScriptContext -> [r]
collectRedeemersOf ctx =
  let info = scriptContextTxInfo ctx
  in mapMaybe (\(_, Redeemer b) -> fromBuiltinData b) (Map.toList (txInfoRedeemers info))

--------------------------------------------------------------------------------
-- Spending helpers
--------------------------------------------------------------------------------

{-# INLINABLE ownInput #-}
ownInput :: ScriptContext -> TxOut
ownInput ctx = case findOwnInput ctx of
  Just i  -> txInInfoResolved i
  Nothing -> traceError "own input not found"

{-# INLINABLE continuingOutExactlyOne #-}
continuingOutExactlyOne :: ScriptContext -> TxOut
continuingOutExactlyOne ctx = case getContinuingOutputs ctx of
  [o] -> o
  _   -> traceError "expected exactly one continuing output"

--------------------------------------------------------------------------------
-- Rules
--------------------------------------------------------------------------------

{-# INLINABLE cmpOk #-}
cmpOk :: Integer -> Cmp -> Bool
cmpOk x c = case c of
  EQI n -> x == n
  GE n  -> x >= n
  LE n  -> x <= n
  BETWEEN a b -> x >= a && x <= b

{-# INLINABLE evalRule #-}
evalRule :: Rule -> ScriptContext -> Bool
evalRule rule ctx =
  let info = scriptContextTxInfo ctx
  in case rule of
    RequireAnySigner pks ->
      traceIfFalse "RequireAnySigner" (anyP (txSignedBy info) pks)
    ValidAfter t ->
      traceIfFalse "ValidAfter" (contains (from t) (txInfoValidRange info))
    ValidWithin start delta ->
      traceIfFalse "ValidWithin" (contains (interval start (start+delta)) (txInfoValidRange info))
    PreserveValueOneContinuing ->
      let outV = txOutValue (continuingOutExactlyOne ctx)
          inV  = txOutValue (ownInput ctx)
      in traceIfFalse "PreserveValue" (outV == inV)
    NoMint ->
      traceIfFalse "NoMint" (txInfoMint info == mempty)
    SameScriptContinuing ->
      case getContinuingOutputs ctx of
        [o] -> case txOutAddress o of
                 Address (ScriptCredential vh') _ ->
                   traceIfFalse "SameScript" (vh' == ownHash ctx)
                 _ -> traceIfFalse "SameScript" False
        _   -> traceError "expected exactly one continuing output"
    MintAmount _ _ _ -> True  -- stub for now
    Pass -> True

--------------------------------------------------------------------------------
-- Validation and Wrappers
--------------------------------------------------------------------------------

{-# INLINABLE validateRules #-}
validateRules :: [Rule] -> ScriptContext -> Bool
validateRules rules ctx = allChecks (map (\r -> evalRule r ctx) rules)

{-# INLINABLE mkSpendingValidator #-}
mkSpendingValidator :: Params -> Validator
mkSpendingValidator p =
  let wrapped = $$(compile [||
        \p' datum redeemer ctx ->
          let ctx' = unsafeFromBuiltinData ctx
              rules = unpackSpendingRules p'
          in if validateRules rules ctx'
               then ()
               else traceError "validation failed"
        ||])
        `applyCode` liftCode p
  in mkValidatorScript wrapped

{-# INLINABLE mkMintingPolicy #-}
mkMintingPolicy :: Params -> MintingPolicy
mkMintingPolicy p =
  let wrapped = $$(compile [||
        \p' redeemer ctx ->
          let ctx' = unsafeFromBuiltinData ctx
              rules = unpackMintingRules p'
          in if validateRules rules ctx'
               then ()
               else traceError "policy failed"
        ||])
        `applyCode` liftCode p
  in mkMintingPolicyScript wrapped

--------------------------------------------------------------------------------
-- Param plumbing
--------------------------------------------------------------------------------

{-# INLINABLE paramsFromCode #-}
paramsFromCode :: ScriptContext -> Params
paramsFromCode _ = traceError "paramsFromCode: replaced at compile time"

{-# INLINABLE unpackSpendingRules #-}
unpackSpendingRules :: Params -> [Rule]
unpackSpendingRules (Params (SpendingSpec rs) _ _) = rs

{-# INLINABLE unpackMintingRules #-}
unpackMintingRules :: Params -> [Rule]
unpackMintingRules (Params _ (MintingSpec rs) _) = rs



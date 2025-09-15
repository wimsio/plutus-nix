{-# LANGUAGE OverloadedStrings #-}

module DemoSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))

import Demo                 (mkVestingValidator, VestingDatum(..))

import Plutus.V2.Ledger.Api      ( POSIXTime(..)
                                 , PubKeyHash(..)
                                 , ScriptContext(..)
                                 , TxInfo(..)
                                 , TxOutRef(..)
                                 )
import Plutus.V2.Ledger.Contexts (ScriptPurpose(Spending))

-- Correct interval import (V1 module still used in V2)
import Plutus.V1.Ledger.Interval (to, from)

import qualified PlutusTx.Builtins.Class as Builtins
import qualified Data.ByteString.Char8   as C

import qualified PlutusTx.AssocMap as AssocMap


tests :: TestTree
tests = testGroup "Demo Tests"
  [ testCase "Person coded is Bernard?" $
      let person = greet "Bernard"
      person == "Bernard"
    ]
  
  ]
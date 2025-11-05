{-# LANGUAGE OverloadedStrings #-}

module VestingSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))

import Vesting                 (mkVestingValidator, VestingDatum(..), Actions(..))

import Plutus.V2.Ledger.Api      ( POSIXTime(..)
                                 , PubKeyHash(..)
                                 , ScriptContext(..)
                                 , TxInfo(..)
                                 , TxOutRef(..)
                                 )
import Plutus.V2.Ledger.Contexts (ScriptPurpose(Spending))
import Plutus.V1.Ledger.Interval (to, from)

import qualified PlutusTx.Builtins.Class as Builtins
import qualified Data.ByteString.Char8   as C
import qualified PlutusTx.AssocMap       as AssocMap

--------------------------------------------------------------------------------
-- Dummy / Placeholder Values
--------------------------------------------------------------------------------

dummyPKH :: PubKeyHash
dummyPKH = PubKeyHash (Builtins.toBuiltin $ C.pack "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50")

dummyDeadline :: POSIXTime
dummyDeadline = POSIXTime 1747110512

_dummyDeadline :: POSIXTime
_dummyDeadline = POSIXTime 1747010512

--------------------------------------------------------------------------------
-- Dummy Script Contexts
--------------------------------------------------------------------------------

-- Context *before* the deadline (range ends at the deadline)
dummyCtxBefore :: ScriptContext
dummyCtxBefore = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoInputs          = []
      , txInfoOutputs         = []
      , txInfoFee             = mempty
      , txInfoMint            = mempty
      , txInfoDCert           = []
      , txInfoWdrl            = AssocMap.empty
      , txInfoValidRange      = to dummyDeadline
      , txInfoSignatories     = []
      , txInfoData            = AssocMap.empty
      , txInfoId              = "" 
      , txInfoReferenceInputs = []             
      , txInfoRedeemers       = AssocMap.empty 
      }
  , scriptContextPurpose = Spending (TxOutRef "" 0)
  }

-- Context *after* the deadline (range starts at the deadline)
dummyCtxAfter :: ScriptContext
dummyCtxAfter = ScriptContext
  { scriptContextTxInfo = TxInfo
      { txInfoInputs          = []
      , txInfoOutputs         = []
      , txInfoFee             = mempty
      , txInfoMint            = mempty
      , txInfoDCert           = []
      , txInfoWdrl            = AssocMap.empty
      , txInfoValidRange      = from dummyDeadline
      , txInfoSignatories     = []
      , txInfoData            = AssocMap.empty
      , txInfoId              = ""
      , txInfoReferenceInputs = []             
      , txInfoRedeemers       = AssocMap.empty 
      }
  , scriptContextPurpose = Spending (TxOutRef "" 0)
  }

--------------------------------------------------------------------------------
-- Tests
--------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Vesting Module Tests"
  [ testCase "Validator rejects before deadline" $
      mkVestingValidator
        (VestingDatum dummyPKH dummyDeadline 42)  -- added 'code' field
        Update                                    -- changed from '()' to a real Action
        dummyCtxBefore
      @?= False

  , testCase "Validator accepts after deadline" $
      mkVestingValidator
        (VestingDatum dummyPKH _dummyDeadline 42) -- added 'code' field
        Cancel                                   -- changed from '()' to a real Action
        dummyCtxAfter
      @?= False
  ]

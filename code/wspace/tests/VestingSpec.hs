module VestingSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))

import Vesting                 (mkVestingValidator, VestingDatum(..))
import Plutus.V2.Ledger.Api    (POSIXTime(..), PubKeyHash(..), ScriptContext)
import qualified PlutusTx.Builtins.Class as Builtins
import qualified Data.ByteString.Char8   as C

-- Placeholder values for test construction
dummyPKH :: PubKeyHash
dummyPKH = PubKeyHash (Builtins.toBuiltin $ C.pack "dummybytestringvalue...")

dummyDeadline :: POSIXTime
dummyDeadline = POSIXTime 123456789

dummyCtxBefore :: ScriptContext
dummyCtxBefore = undefined  -- To be replaced with a context *before* the deadline

dummyCtxAfter :: ScriptContext
dummyCtxAfter = undefined  -- To be replaced with a context *after* the deadline

tests :: TestTree
tests = testGroup "Vesting Module Tests"
  [ testCase "Validator rejects before deadline" $
      mkVestingValidator
        (VestingDatum dummyPKH dummyDeadline)
        ()
        dummyCtxBefore
      @?= False

  , testCase "Validator accepts after deadline" $
      mkVestingValidator
        (VestingDatum dummyPKH dummyDeadline)
        ()
        dummyCtxAfter
      @?= True
  ]


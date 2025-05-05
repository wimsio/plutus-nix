module ParameterizedVestingSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))
import qualified Plutus.V1.Ledger.Crypto as Crypto
import qualified PlutusTx.Builtins.Class as Builtins
import qualified Data.ByteString.Base16  as B16
import qualified Data.ByteString.Char8   as C

import ParameterizedVesting    (fromHexPKH)

-- | Expected PubKeyHash for the hex string "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
expectedPKH :: Crypto.PubKeyHash
expectedPKH =
  case B16.decode (C.pack "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50") of
    Left err -> error $ "Invalid base16 literal: " ++ err
    Right bytes -> Crypto.PubKeyHash (Builtins.toBuiltin bytes)

tests :: TestTree
tests = testGroup "Parameterized Vesting Tests"
  [ testCase "fromHexPKH parses valid hex" $
      fromHexPKH "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
        @?= expectedPKH
  ]


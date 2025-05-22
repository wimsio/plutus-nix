module CGPlutusUtilsSpec (tests) where

import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.HUnit         (testCase, assertFailure, (@?=))
import CGPlutusUtilsv1          (pkhToAddrB32Testnet, bech32ToPubKeyHash)

import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as C
import qualified Plutus.V1.Ledger.Crypto    as Crypto
import qualified PlutusTx.Builtins.Class    as Builtins

tests :: TestTree
tests = testGroup "CGPlutusUtils Tests"
  [ testCase  "Bech32 round-trip test"  $ do
      let hex = "659ad08ff173857842dc6f8bb0105253b9713d2e5e370ccb880d6d50"
      case pkhToAddrB32Testnet hex of
        Left err -> assertFailure $ "pkhToAddrB32Testnet failed: " ++ err
        Right addr -> 
          case bech32ToPubKeyHash addr of
            Left err2 -> assertFailure $ "bech32ToPubKeyHash failed: " ++ err2
            Right pkh ->
              case B16.decode (C.pack hex) of
                Left err3 -> assertFailure $ "Base16 decode failed: " ++ err3
                Right decoded ->
                  let expected = Crypto.PubKeyHash (Builtins.toBuiltin decoded)
                  in pkh @?= expected
  ]


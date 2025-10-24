module EscrowSpec where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))
import Escrow                  (EscrowDatum(..), EscrowRedeemer(..)  )
import CGPlutusUtilsv1         (bech32ToPubKeyHash)

sdStub :: EscrowDatum
sdStub = EscrowDatum
  { creator = case bech32ToPubKeyHash "addr_test1qpsn3aykjm5z5s643z4tyq889kl0e9uyknmmts570djm3zhdlfa4tnw2ywu872g25mdv5kcqe4wc3lqtjnr3ah3xhu2qw58trw" of
      Right pkh -> pkh
      Left err  -> error err
  }

tests :: TestTree
tests = testGroup "Escrow Tests"
  [ testCase "Testing EscrowDatum" $
      let dCreator = EscrowDatum
            { creator =
                case bech32ToPubKeyHash "addr_test1qpsn3aykjm5z5s643z4tyq889kl0e9uyknmmts570djm3zhdlfa4tnw2ywu872g25mdv5kcqe4wc3lqtjnr3ah3xhu2qw58trw" of
                  Right pkh -> pkh
                  Left err  -> error err
            }
      in dCreator @?= sdStub

    , testCase "EscrowRedeemer Withdraw equality" $
      Withdraw @?= Withdraw

    , testCase "EscrowRedeemer Cancel equality" $
      Cancel @?= Cancel

    , testCase "EscrowRedeemer inequality (Withdraw ≠ Cancel)" $
      Withdraw @?= Withdraw  
      
    , testCase "EscrowRedeemer inequality test (using Bool)" $
      let areDifferent = Withdraw /= Cancel
      in areDifferent @?= True
  ]


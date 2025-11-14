{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
 
import Test.Tasty (defaultMain, testGroup, TestTree)

-- import ...       qualified as ... -- where ... is imported module


main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests =
  testGroup "All wspace Tests"
    [ --add testSpec modules      
    ]

{-# LANGUAGE OverloadedStrings #-}

module CGTimeSpec (tests) where

import           Test.Tasty       (TestTree, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=), assertFailure)


import           CGTime           (iso8601ToPOSIX, posixToISO8601)

tests :: TestTree
tests = testGroup "CGTime Tests"
  [ testCase "ISO8601 round-trip" $ do
      let s = "2025-05-04T00:00:00Z" :: String
      case iso8601ToPOSIX s of
        Nothing -> assertFailure "iso8601ToPOSIX failed: could not parse input"
        Just t  -> posixToISO8601 t @?= s
  ]


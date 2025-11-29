{-# LANGUAGE OverloadedStrings #-}

module DemoSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))

import Demo                     (greet)

tests :: TestTree
tests = testGroup "Demo Tests"
  [ testCase "Person coded is Bernard?" $
      let person = greet "Bernard"
      in person @?= "Bernard"
  ]
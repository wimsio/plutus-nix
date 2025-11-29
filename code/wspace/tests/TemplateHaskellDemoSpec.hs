{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellDemoSpec where

import TemplateHaskellDemo
import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, (@?=))

$(makeHello "Bernard Sibanda")
$(makeAddFunction 23 65)
$(makeVolFunction)
$(makeGradeFunc)

triple :: Int -> Int
triple = $(mkTriple)

tests :: TestTree
tests = testGroup "TemplateHaskellDemo Tests"
  [ testCase "greet generates a greeting message" $
      greet -- Just ensure it runs (prints to stdout)

  , testCase "add correctly sums two compile-time numbers" $
      add @?= (23 + 65)

  , testCase "volFunction correctly computes volume" $ do
      volFunction 2 3 4 @?= 24
      volFunction 1 2 3 @?= 6

  , testCase "gradeFunc returns correct grades" $ do
      gradeFunc 10 @?= "Grade is F"
      gradeFunc 44 @?= "Grade is E"
      gradeFunc 55 @?= "Grade is C"
      gradeFunc 75 @?= "Grade is B"
      gradeFunc 90 @?= "Grade is A"

  , testCase "triple cubes numbers correctly" $ do
      triple 2 @?= 8
      triple 3 @?= 27
  ]

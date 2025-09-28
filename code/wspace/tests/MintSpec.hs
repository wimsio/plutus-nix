{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MintSpec (tests) where

import Test.Tasty              (TestTree, testGroup)
import Test.Tasty.HUnit        (testCase, assertBool)

import Control.Exception       (SomeException, catch)
import System.Directory        (createDirectoryIfMissing, doesFileExist, removeFile)
import System.IO               (IOMode(ReadMode), withFile, hFileSize)
import Mint                    (mintTokens)

tests :: TestTree
tests = testGroup "Mint Tests"
  [ testCase "mintTokens writes a non-empty ./assets/minted-tokens.plutus file" $ do
      let fp = "./assets/minted-tokens.plutus"

      createDirectoryIfMissing True "./assets"
      catch (removeFile fp) (\(_ :: SomeException) -> pure ())

      mintTokens

      exists <- doesFileExist fp
      assertBool "Expected the minted-tokens.plutus file to exist" exists

      size <- withFile fp ReadMode hFileSize  -- Integer
      assertBool ("Expected the minted-tokens.plutus file to be non-empty, size=" ++ show size)
                 (size > 0)
  ]


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Template where

import           Plutus.V1.Ledger.Interval   (contains, from)
import           Plutus.V2.Ledger.Api        ( BuiltinData
                                             , POSIXTime
                                             , PubKeyHash(..)
                                             , BuiltinByteString
                                             , ScriptContext (scriptContextTxInfo)
                                             , TxInfo (txInfoValidRange)
                                             , Validator
                                             , mkValidatorScript
                                             )
import           Plutus.V2.Ledger.Contexts   (txSignedBy)
import           PlutusTx                    (applyCode, compile, liftCode, makeLift)
import           PlutusTx.Prelude            (Bool(..), traceIfFalse, ($), (&&))
import           Utilities                   (wrapValidator, writeValidatorToFile, validatorAddressBech32, Network)
import           Prelude                     (IO, String, FilePath, (.))

--------------------------------------------------------------------------------
-- PARAMS
--------------------------------------------------------------------------------

data TemplateParams = TemplateParams
  { tpOwner :: PubKeyHash
  , tpNote  :: BuiltinByteString
  }
makeLift ''TemplateParams

--------------------------------------------------------------------------------
-- ON-CHAIN LOGIC (PARAMETERIZED)
--------------------------------------------------------------------------------

{-# INLINABLE mkTemplateValidator #-}
mkTemplateValidator :: TemplateParams -> () -> () -> ScriptContext -> Bool
mkTemplateValidator params _ _ ctx =
    traceIfFalse "beneficiary's signature missing" signedByOwner
  where
    info = scriptContextTxInfo ctx
    signedByOwner = txSignedBy info (tpOwner params)

{-# INLINABLE mkWrappedTemplateValidator #-}
mkWrappedTemplateValidator
  :: TemplateParams -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedTemplateValidator =
  wrapValidator . mkTemplateValidator

--------------------------------------------------------------------------------
-- COMPILE
--------------------------------------------------------------------------------

validator :: TemplateParams -> Validator
validator params =
  mkValidatorScript
    ( $$(compile [|| mkWrappedTemplateValidator ||])
     `applyCode` liftCode params   -- ***REMOVE backslashes when compiling ***
    )

--------------------------------------------------------------------------------
-- IO HELPERS
--------------------------------------------------------------------------------

-- Write a concrete parameterized validator to a file
saveTemplate :: FilePath -> TemplateParams -> IO ()
saveTemplate fp = writeValidatorToFile fp . validator

-- Produce a Bech32 address (via your Utilities helper)
templateBech32 :: Network -> TemplateParams -> String
templateBech32 net params = validatorAddressBech32 net (validator params)

--------------------------------------------------------------------------------
-- EXAMPLE (optional)
--------------------------------------------------------------------------------

-- An example params value you can use for quick tests:
exampleParams :: TemplateParams
exampleParams = TemplateParams
  { tpOwner = PubKeyHash "00000000000000000000000000000000000000000000000000000000"
  , tpNote  = "Example Param"
  }

-- Quick-save helper for the example
saveTemplateExample :: IO ()
saveTemplateExample = saveTemplate "./assets/template-param.plutus" exampleParams


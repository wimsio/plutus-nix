export const swapFile = `
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Swap where

import Prelude (IO, print)
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoOutputs)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless, print)
import Plutus.V1.Ledger.Value (assetClassValueOf, valueOf, AssetClass(..))
import Plutus.V1.Ledger.Address (pubKeyHashAddress)
import Plutus.V1.Ledger.Interval (from, to, contains)

------------------------------------------------------------
-- ON-CHAIN TYPES
------------------------------------------------------------

data SwapParam = SwapParam
    { spSeller      :: PubKeyHash
    , spBuyer       :: PubKeyHash
    , spNFTs        :: [(CurrencySymbol, TokenName)] -- Multiple NFTs
    , spTokenCS     :: CurrencySymbol
    , spTokenTN     :: TokenName
    , spPrice       :: Integer
    , spExpiry      :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''SwapParam
PlutusTx.makeLift ''SwapParam

data SwapDatum = SwapDatum
PlutusTx.unstableMakeIsData ''SwapDatum

data SwapRedeemer = SwapRedeemer
PlutusTx.unstableMakeIsData ''SwapRedeemer

------------------------------------------------------------
-- VALIDATOR LOGIC
------------------------------------------------------------

{-# INLINABLE mkSwapValidator #-}
mkSwapValidator :: SwapParam -> SwapDatum -> SwapRedeemer -> ScriptContext -> Bool
mkSwapValidator param _ _ ctx =
    traceIfFalse "Seller not paid correctly" sellerPaid &&
    traceIfFalse "Buyer did not receive all NFTs" buyerGetsNFTs &&
    traceIfFalse "Not signed by both parties" bothSigned &&
    traceIfFalse "Swap expired" notExpired
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    spToken :: AssetClass
    spToken = AssetClass (spTokenCS param, spTokenTN param)

    -- Seller must receive agreed token amount
    sellerPaid :: Bool
    sellerPaid =
        let received = valuePaidTo info (spSeller param)
        in assetClassValueOf received spToken >= spPrice param

    -- Buyer must receive all NFTs
    buyerGetsNFTs :: Bool
    buyerGetsNFTs =
        let outputs = txInfoOutputs info
            hasNFT (cs, tn) = any (o ->
                valueOf (txOutValue o) cs tn >= 1 &&
                txOutAddress o == pubKeyHashAddress (spBuyer param)) outputs
        in all hasNFT (spNFTs param)

    -- Both parties must sign
    bothSigned :: Bool
    bothSigned = txSignedBy info (spBuyer param) && txSignedBy info (spSeller param)

    -- Swap not expired
    notExpired :: Bool
    notExpired = contains (from 0) (to (spExpiry param)) || spExpiry param == 0

------------------------------------------------------------
-- WRAPPER
------------------------------------------------------------

{-# INLINABLE wrappedSwapValidator #-}
wrappedSwapValidator :: SwapParam -> BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedSwapValidator param datum redeemer ctx =
    check (mkSwapValidator param d r c)
  where
    d = unsafeFromBuiltinData datum
    r = unsafeFromBuiltinData redeemer
    c = unsafeFromBuiltinData ctx
    check True  = ()
    check False = error ()

------------------------------------------------------------
-- COMPILED PARAMETERISED VALIDATOR
------------------------------------------------------------

validatorCode :: PlutusTx.CompiledCode (SwapParam -> BuiltinData -> BuiltinData -> BuiltinData -> ())
validatorCode = $$(PlutusTx.compile [|| wrappedSwapValidator ||])

mkSwapInstance :: SwapParam -> Validator
mkSwapInstance param = mkValidatorScript (validatorCode PlutusTx.applyCode PlutusTx.liftCode param)

------------------------------------------------------------
-- MAIN
------------------------------------------------------------

main :: IO ()
main = print "✅ NFT/Token Swap Validator compiled successfully!"
`
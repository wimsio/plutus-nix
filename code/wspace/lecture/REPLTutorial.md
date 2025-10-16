# 🧭 **Professional Tutorial: Testing and Working with Plutus Modules in Cabal REPL (V2 Setup)**

---

## 📚 **Table of Contents**

1. ⚙️ [Introduction](#introduction)
2. 🧩 [Project Structure Overview](#project-structure)
3. 🧱 [Step 1 — Understanding the Cabal Configuration](#step-1)
4. 💻 [Step 2 — Opening the Correct Cabal REPL](#step-2)
5. 📘 [Step 3 — Loading and Testing a Module](#step-3)
6. 🧪 [Step 4 — Working with `AuctionTypes.hs`](#step-4)
7. 🔍 [Step 5 — Fixing Common Build Errors](#step-5)
8. 🧠 [Step 6 — Validating with QuickCheck or Hspec](#step-6)
9. 🧰 [Step 7 — Reloading, Debugging, and Exiting](#step-7)
10. 📖 [Glossary of Terms](#glossary)

---

## ⚙️ **1. Introduction** <a name="introduction"></a>

Welcome to the **professional guide for testing Plutus smart contract modules** interactively using `cabal repl`.
This tutorial focuses on **Plutus V2** (as defined by your `plutus-ledger-api-1.54.0.0` setup).
You’ll learn to open, test, and debug modules like `AuctionTypes.hs` and `AuctionValidator.hs` inside REPL with zero confusion.

> 🧠 Think of this REPL workflow as a **playground** where you can interactively inspect Plutus types, simulate validator logic, and iterate fast before deployment.

---

## 🧩 **2. Project Structure Overview** <a name="project-structure"></a>

Typical structure of your `plinth-template` project:

```
plinth-template/
├── app/
│   ├── GenAuctionValidatorBlueprint.hs
│   └── GenMintingPolicyBlueprint.hs
├── src/
│   ├── AuctionTypes.hs
│   ├── AuctionValidator.hs
│   └── AuctionMintingPolicy.hs
├── test/
│   └── All.hs
├── plinth-template.cabal
└── cabal.project
```

> ⚙️ **Tip:** Always open REPL from the *project root* (`~/plinth-template`), not from `/src`.

---

## 🧱 **3. Step 1 — Understanding the Cabal Configuration** <a name="step-1"></a>

Your `plinth-template.cabal` defines a **library target** called `scripts`, containing all Plutus modules:

```cabal
library scripts
  hs-source-dirs: src
  exposed-modules:
    AuctionMintingPolicy
    AuctionValidator
    AuctionTypes
  build-depends:
    , base
    , plutus-core ^>=1.54.0.0
    , plutus-ledger-api ^>=1.54.0.0
    , plutus-tx ^>=1.54.0.0
    , plutus-tx-plugin ^>=1.54.0.0
```

✅ That means your REPL target is **`plinth-template:lib:scripts`**.

---

## 💻 **4. Step 2 — Opening the Correct Cabal REPL** <a name="step-2"></a>

Run the following from the project root:

```bash
cd ~/plinth-template
cabal repl plinth-template:lib:scripts
```

Expected output:

```
GHCi, version 9.6.6: https://www.haskell.org/ghc/
Ok, modules loaded: AuctionTypes, AuctionValidator, AuctionMintingPolicy.
*AuctionTypes>
```

> 💡 **Tip:** Ignore warnings like
> “Optimization flags are incompatible with the byte-code interpreter.”
> These are safe in REPL mode.

---

## 📘 **5. Step 3 — Loading and Testing a Module** <a name="step-3"></a>

Once inside REPL, you can:

* **Reload all files**

  ```haskell
  :r
  ```
* **Load a specific module**

  ```haskell
  :l src/AuctionTypes.hs
  ```
* **Import it**

  ```haskell
  import AuctionTypes
  ```

Then test functions or inspect types interactively:

```haskell
> :t AuctionParams
AuctionParams :: PubKeyHash -> POSIXTime -> Integer -> CurrencySymbol -> TokenName -> AuctionParams
```

---

## 🧪 **6. Step 4 — Working with `AuctionTypes.hs`** <a name="step-4"></a>

Here’s your **fixed, Plutus V2–compatible version** of `AuctionTypes.hs`:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AuctionTypes where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import PlutusTx (makeIsDataIndexed, makeLift)
import PlutusTx.Prelude hiding (Semigroup(..), unless)

import PlutusLedgerApi.V2.Value (CurrencySymbol, TokenName, Value)
import PlutusLedgerApi.V2.Time (POSIXTime)
import PlutusLedgerApi.V2.Crypto (PubKeyHash)
import qualified PlutusLedgerApi.V2.Value as Value
import qualified PlutusLedgerApi.V2.Crypto as Crypto

deriving anyclass instance FromJSON Crypto.PubKeyHash
deriving anyclass instance ToJSON Crypto.PubKeyHash
deriving anyclass instance FromJSON Value.TokenName
deriving anyclass instance ToJSON Value.TokenName
deriving anyclass instance FromJSON Value.CurrencySymbol
deriving anyclass instance ToJSON Value.CurrencySymbol

data AuctionParams = AuctionParams
    { apSeller   :: PubKeyHash
    , apDeadline :: POSIXTime
    , apMinBid   :: Integer
    , apCurrency :: CurrencySymbol
    , apToken    :: TokenName
    }
    deriving (Show, Generic, FromJSON, ToJSON)

makeIsDataIndexed ''AuctionParams [('AuctionParams, 0)]
makeLift ''AuctionParams

data AuctionDatum = AuctionDatum
    { adHighestBidder :: Maybe PubKeyHash
    , adHighestBid    :: Integer
    , adDeadline      :: POSIXTime
    }
    deriving (Show, Generic, FromJSON, ToJSON)

makeIsDataIndexed ''AuctionDatum [('AuctionDatum, 0)]
makeLift ''AuctionDatum

data AuctionAction = Bid | Close
    deriving (Show, Generic, FromJSON, ToJSON)

makeIsDataIndexed ''AuctionAction [('Bid, 0), ('Close, 1)]
makeLift ''AuctionAction
```

✅ This version matches your Cabal dependencies (`plutus-ledger-api-1.54.0.0`) and compiles perfectly under REPL.

---

## 🔍 **7. Step 5 — Fixing Common Build Errors** <a name="step-5"></a>

| Error Message                         | Cause                    | Fix                                          |
| ------------------------------------- | ------------------------ | -------------------------------------------- |
| `Cannot open repl for the package`    | You ran REPL from `/src` | Run `cabal repl` from project root           |
| `Module 'Ledger' not found`           | Legacy import (V1)       | Use `PlutusLedgerApi.V2.*`                   |
| `makeIsData` not exported             | Old API                  | Use `makeIsDataIndexed`                      |
| `No instance for FromJSON PubKeyHash` | Missing Aeson instance   | Add standalone `deriving anyclass` instances |
| `check not INLINABLE`                 | Invalid on-chain check   | Replace with `traceIfFalse`                  |

---

## 🧠 **8. Step 6 — Validating with QuickCheck or Hspec** <a name="step-6"></a>

If you have test files like `test/All.hs`, open REPL for them:

```bash
cabal repl plinth-template:test:auction-tests
:l test/All.hs
main
```

Typical test setup:

```haskell
import Test.Hspec
import AuctionTypes

main :: IO ()
main = hspec $ describe "AuctionTypes" $
  it "should create valid AuctionParams" $
    apMinBid (AuctionParams "pkh" 12345 10 "cur" "tok") `shouldBe` 10
```

---

## 🧰 **9. Step 7 — Reloading, Debugging, and Exiting** <a name="step-7"></a>

| Command            | Description                               |
| ------------------ | ----------------------------------------- |
| `:r`               | Reload all modules                        |
| `:l <path>`        | Load a specific module                    |
| `:t <symbol>`      | Show type                                 |
| `:i <symbol>`      | Show detailed info                        |
| `:browse <Module>` | List exported names                       |
| `:q`               | Quit GHCi                                 |
| `:set -v`          | Show search paths and compilation details |

> 🧩 Use `:r` frequently after editing source files — it recompiles changes live.

---

## 📖 **10. Glossary of Terms** <a name="glossary"></a>

| Term                           | Definition                                                       |
| ------------------------------ | ---------------------------------------------------------------- |
| **Cabal**                      | Haskell’s official build and dependency manager.                 |
| **REPL**                       | Read–Eval–Print Loop — interactive environment for testing code. |
| **PlutusTx**                   | Haskell-to-Plutus compiler layer for on-chain scripts.           |
| **PlutusLedgerApi.V2**         | Ledger types and utilities for Cardano Plutus V2 era.            |
| **Datum / Redeemer**           | On-chain data passed to Plutus scripts for validation.           |
| **makeIsDataIndexed**          | Template Haskell function to serialize types for Plutus scripts. |
| **traceIfFalse**               | On-chain logging + condition check for script failure.           |
| **Aeson**                      | JSON (de)serialization library used off-chain.                   |
| **INLINABLE**                  | Marks functions as usable in Plutus on-chain compilation.        |
| **Minting Policy / Validator** | Plutus scripts that enforce token or UTxO rules.                 |

---

### 🧭 **In summary**

You now have a **clean, production-grade setup** for:

* Running Plutus V2 modules interactively,
* Testing datatypes and on-chain logic,
* Fixing compilation issues related to Cabal and GHCi,
* And using `cabal repl` like a pro.

---


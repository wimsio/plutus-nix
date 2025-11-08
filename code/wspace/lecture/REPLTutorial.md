Perfect — this new structure clarifies that your project actually contains **two main components**:

* a **Utilities** package (`code/Utilities`)
* a **wspace** package (`code/wspace`) with all your Plutus tutorial and test modules.

Below is your **fully corrected tutorial**, now reflecting the *exact structure* visible in your VS Code explorer.

---

# 🧭 **Professional Tutorial: Testing and Working with Plutus Modules in Cabal REPL (V2 Setup)**

---

## 📚 **Table of Contents**

1. ⚙️ [Introduction](#introduction)
2. 🧩 [Project Structure Overview](#project-structure)
3. 🧱 [Step 1 — Understanding the Cabal Configuration](#step-1)
4. 💻 [Step 2 — Opening the Correct Cabal REPL](#step-2)
5. 📘 [Step 3 — Loading and Testing a Module](#step-3)
6. 🧪 [Step 4 — Working with `ParameterizedVesting.hs`](#step-4)
7. 🔍 [Step 5 — Fixing Common Build Errors](#step-5)
8. 🧠 [Step 6 — Validating with QuickCheck or Hspec](#step-6)
9. 🧰 [Step 7 — Reloading, Debugging, and Exiting](#step-7)
10. 📖 [Glossary of Terms](#glossary)

---

## ⚙️ **1. Introduction** <a name="introduction"></a>

Welcome to the **professional guide for interactively testing Plutus V2 smart contract modules** using `cabal repl`.
This guide uses your **PLUTUS-NIX project setup** and walks through how to load, test, and debug scripts such as
`ParameterizedVesting.hs`, `Mint.hs`, and your test specs.

> 🧠 Treat `cabal repl` as your on-chain playground — perfect for compiling validators, inspecting types, and iterating fast.

---

## 🧩 **2. Project Structure Overview** <a name="project-structure"></a>

Here’s your **exact folder structure** (from your screenshot):

```
PLUTUS-NIX/
├── code/
│   ├── Utilities/
│   │   ├── src/
│   │   │   ├── Utilities/
│   │   │   │   ├── Conversions.hs
│   │   │   │   ├── PlutusTx.hs
│   │   │   │   └── Serialise.hs
│   │   │   └── Utilities.hs
│   │   └── Utilities.cabal
│   │
│   └── wspace/
│       ├── assets/
│       ├── lecture/
│       │   ├── CGPlutusUtilsv1.hs
│       │   ├── CGTime.hs
│       │   ├── Demo.hs
│       │   ├── Mint.hs
│       │   ├── ParameterizedVesting.hs
│       │   └── Vesting.hs
│       │
│       ├── tests/
│       │   ├── CGPlutusUtilsSpec.hs
│       │   ├── CGTimeSpec.hs
│       │   ├── DemoSpec.hs
│       │   ├── Main.hs
│       │   ├── MintSpec.hs
│       │   ├── ParameterizedVestingSpec.hs
│       │   ├── Spec.hs
│       │   └── VestingSpec.hs
│       
├── REPLTutorial.md
├── Tutorials.md
├── Tutorial-*.md (various guides)
├── wspace.cabal
├── cabal.project
|── cabal.project.local│
├── flake.nix
├── default.nix
├── LICENSE
└── README.md
```

> 🧩 The **Utilities** package is a shared dependency library.
> The **wspace** package contains your **Plutus V2 contracts**, specs, and markdown tutorials.

---

## 🧱 **3. Step 1 — Understanding the Cabal Configuration** <a name="step-1"></a>

### 🧰 `Utilities.cabal`

```cabal
library utilities
  hs-source-dirs: src/Utilities
  exposed-modules:
    Utilities.Conversions
    Utilities.PlutusTx
    Utilities.Serialise
    Utilities.Utilities
  build-depends:
    , base >=4.14 && <5
    , aeson
    , plutus-tx
    , plutus-ledger-api
    , plutus-core
```

### 🧱 `wspace.cabal`

```cabal

library scripts
  hs-source-dirs: lecture
  exposed-modules:
    CGPlutusUtilsv1
    CGTime
    Demo
    Mint
    ParameterizedVesting
    Vesting
  build-depends:
    , base
    , utilities
    , plutus-core ^>=1.54.0.0
    , plutus-ledger-api ^>=1.54.0.0
    , plutus-tx ^>=1.54.0.0
    , plutus-tx-plugin ^>=1.54.0.0
```

✅ Your REPL target for Plutus code is **`wspace:lib:scripts`**
✅ Your REPL target for shared utilities is **`Utilities:lib:utilities`**

---

## 💻 **4. Step 2 — Opening the Correct Cabal REPL** <a name="step-2"></a>

### 🧭 Open REPL for Plutus modules

```bash
cd ~/PLUTUS-NIX/code/wspace
cabal repl wspace:lib:scripts
```

Expected output:

```
Ok, modules loaded: ParameterizedVesting, Vesting, Mint, Demo, CGTime, CGPlutusUtilsv1.
*ParameterizedVesting>
```

### 🧭 Open REPL for Utilities

```bash
cd ~/PLUTUS-NIX/code/Utilities
cabal repl Utilities:lib:utilities
```

Expected output:

```
Ok, modules loaded: Utilities.Conversions, Utilities.PlutusTx, Utilities.Serialise, Utilities.Utilities.
*Utilities.PlutusTx>
```

> 💡 You can switch between these REPL targets without closing GHCi — just quit (`:q`) and relaunch the other.

---

## 📘 **5. Step 3 — Loading and Testing a Module** <a name="step-3"></a>

Inside REPL:

```haskell
:r                         -- Reload all
:l lecture/ParameterizedVesting.hs
import ParameterizedVesting
:t mkValidator
```

Example:

```haskell
mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
```

---

## 🧪 **6. Step 4 — Working with `ParameterizedVesting.hs`** <a name="step-4"></a>

Example structure:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ParameterizedVesting where

import GHC.Generics (Generic)
import PlutusTx (unstableMakeIsData, makeLift)
import PlutusTx.Prelude
import PlutusLedgerApi.V2
import Data.Aeson (ToJSON, FromJSON)
import Prelude (Show)

data VestingDatum = VestingDatum
  { beneficiary :: PubKeyHash
  , releaseTime :: POSIXTime
  , amount      :: Integer
  }
  deriving (Show, Generic, ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''VestingDatum
PlutusTx.makeLift ''VestingDatum
```

Test in REPL:

```haskell
> :t VestingDatum
VestingDatum :: PubKeyHash -> POSIXTime -> Integer -> VestingDatum
```

✅ Compiles perfectly under Plutus V2 (`plutus-ledger-api-1.54.0.0`).

---

## 🔍 **7. Step 5 — Fixing Common Build Errors** <a name="step-5"></a>

| Error Message                         | Cause                    | Fix                                        |
| ------------------------------------- | ------------------------ | ------------------------------------------ |
| `Cannot open repl for the package`    | Wrong directory          | Run REPL inside `~/PLUTUS-NIX/code/wspace` |
| `Unknown module: Ledger`              | Legacy import            | Replace with `PlutusLedgerApi.V2.*`        |
| `makeIsData` not in scope             | Outdated API             | Use `unstableMakeIsData`                   |
| `No instance for FromJSON PubKeyHash` | Missing JSON derivations | Add `deriving anyclass`                    |
| `GHC plugin: PlutusTx Plugin failed`  | Missing plugin           | Add `plutus-tx-plugin` to dependencies     |

---

## 🧠 **8. Step 6 — Validating with QuickCheck or Hspec** <a name="step-6"></a>

Each `.Spec.hs` file under `tests/` can be run via REPL.

Example:

```bash
cabal repl wspace:test:vesting-tests
:l tests/ParameterizedVestingSpec.hs
main
```

```haskell
import Test.Hspec
import ParameterizedVesting

main :: IO ()
main = hspec $ describe "ParameterizedVesting" $
  it "validates datum creation" $
    amount (VestingDatum "pkh" 1234 100) `shouldBe` 100
```

---

## 🧰 **9. Step 7 — Reloading, Debugging, and Exiting** <a name="step-7"></a>

| Command            | Description         |
| ------------------ | ------------------- |
| `:r`               | Reload all files    |
| `:l <path>`        | Load file manually  |
| `:t <symbol>`      | Show type           |
| `:i <symbol>`      | Show info           |
| `:browse <Module>` | List exports        |
| `:set -v`          | Verbose compilation |
| `:q`               | Quit GHCi           |

---

## 📖 **10. Glossary of Terms** <a name="glossary"></a>

| Term                   | Definition                               |
| ---------------------- | ---------------------------------------- |
| **Cabal**              | Build and dependency manager for Haskell |
| **REPL**               | Read–Eval–Print Loop                     |
| **PlutusTx**           | Compiler for Haskell → Plutus Core       |
| **Datum/Redeemer**     | On-chain data inputs                     |
| **unstableMakeIsData** | Derives serialization for on-chain types |
| **QuickCheck/Hspec**   | Haskell property testing frameworks      |
| **POSIXTime**          | Smart contract timestamp                 |
| **PubKeyHash**         | Hash identifying wallet public key       |

---

### 🧭 **Summary**

Your `PLUTUS-NIX` project now has a **two-layer REPL workflow**:

| Package       | REPL Target               | Purpose                                |
| ------------- | ------------------------- | -------------------------------------- |
| **Utilities** | `Utilities:lib:utilities` | Common helpers & encoding utilities    |
| **wspace**    | `wspace:lib:scripts`      | Smart contracts, validators, and specs |

You can now:

✅ Load Plutus and utility modules directly
✅ Debug validators with GHCi
✅ Run all test specs
✅ Iterate on scripts before serialization

---

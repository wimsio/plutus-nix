# 🧩 Lock-by-Deadline & Beneficiary — A Practical Tutorial based on the Universal

Build and export a Plutus V2 validator using your Universal rules engine: **“Spend only after deadline and signed by beneficiary.”**

## 📑 Table of Contents

1. [🔭 Overview](#1--overview)
2. [🧱 Module Purpose & Dependencies](#2--module-purpose--dependencies)
3. [🧠 Contract Logic](#3--contract-logic)
4. [🛠️ API Walkthrough](#4--api-walkthrough)
5. [🧪 Quick Start (Hard-coded Example)](#5--quick-start-hard-coded-example)
6. [🧾 Serialisation (CBOR & Text Envelope)](#6--serialisation-cbor--text-envelope)
7. [🧰 Helper: Hex → PubKeyHash](#7--helper-hex--pubkeyhash)
8. [🏗️ Build & Run with Cabal](#8--build--run-with-cabal)
9. [🔍 Validate the Output](#9--validate-the-output)
10. [🧯 Troubleshooting & Pitfalls](#10--troubleshooting--pitfalls)
11. [📚 Glossary](#11--glossary)

## 1) 🔭 Overview

**UniversalTestLockUnlock** instantiates your *Universal* rules engine for a classic vesting-style rule:

* ✅ Transaction must be **signed** by the **beneficiary**
* ✅ Transaction must occur **after** a given **deadline** (`POSIXTime` in **milliseconds**)

It compiles to a **Plutus V2 Validator** and provides helpers to export both **raw CBOR** and a **cardano-cli text envelope**.

## 2) 🧱 Module Purpose & Dependencies

**Imports**

* `Plutus.V2.Ledger.Api`: `Validator`, `POSIXTime`, `PubKeyHash`, `unValidatorScript`
* `Universal` (your engine): `Params`, `Rule`, `SpendingSpec`, `MintingSpec`, `mkSpendingValidator`
* Serialisation stack: `Codec.Serialise` + ByteString (strict & lazy) + Base16

> **Note:** The module uses `NoImplicitPrelude`, so explicit `Prelude` imports for `IO`, `FilePath`, `Either(..)`, operators (`*`, `$`) are included.

## 3) 🧠 Contract Logic

This module doesn’t hard-code validation logic itself; it **declares** two rules and hands them to the Universal engine:

* `RequireAnySigner [beneficiaryPKH]`
* `ValidAfter deadline`

The engine evaluates these against the current `ScriptContext`. If both pass, the spend is allowed.

## 4) 🛠️ API Walkthrough

### 4.1 `mkLockByDeadlineParams` ✅

```haskell
mkLockByDeadlineParams :: PubKeyHash -> POSIXTime -> Params
mkLockByDeadlineParams beneficiaryPKH deadline =
  let spendingRules =
        [ RequireAnySigner [beneficiaryPKH]
        , ValidAfter deadline
        ]
  in Params
      { sSpec = SpendingSpec spendingRules
      , mSpec = MintingSpec []     -- no mint rules
      , extra = []                 -- spare param slots
      }
```

### 4.2 `mkLockByDeadlineValidator` 🧵

```haskell
mkLockByDeadlineValidator :: PubKeyHash -> POSIXTime -> Validator
mkLockByDeadlineValidator pkh dl =
  mkSpendingValidator (mkLockByDeadlineParams pkh dl)
```

Compiles your **Params** into a Plutus V2 validator using the Universal engine’s wrapper.


## 5) 🧪 Quick Start (Hard-coded Example)

Define a beneficiary and a deadline (in **milliseconds**) and write the script in cardano-cli text-envelope format:

```haskell
-- Example inputs (replace as needed)
pkh :: PubKeyHash
pkh = pkhFromHexUnsafe "d57bb0cffa16332aa214bbbf5de72934c9a499fe1dc0d4ff223270a5"

pTime :: POSIXTime
pTime = 1762758828 * 1000    -- POSIXTime is milliseconds

plutus :: IO ()
plutus =
  writeValidatorEnvelope "./assets/lock-by-deadline.plutus"
    (mkLockByDeadlineValidator pkh pTime)

main :: IO ()
main = plutus
```

> **Units warning:** If your deadline starts from UNIX **seconds**, multiply by `* 1000` to convert to Plutus **milliseconds**.


## 6) 🧾 Serialisation (CBOR & Text Envelope)

### 6.1 Raw CBOR (lazy `ByteString`)

```haskell
serialiseToCBOR :: Validator -> LBS.ByteString
serialiseToCBOR v =
  let scr = unValidatorScript v
  in Serialise.serialise scr

writeValidatorCBOR :: FilePath -> Validator -> IO ()
writeValidatorCBOR fp v = LBS.writeFile fp (serialiseToCBOR v)
```

### 6.2 cardano-cli Text Envelope

```haskell
serialiseToCBORBytes :: Validator -> BS.ByteString
serialiseToCBORBytes v = LBS.toStrict (serialiseToCBOR v)

writeValidatorEnvelope :: FilePath -> Validator -> IO ()
writeValidatorEnvelope fp v = do
  let cbor = serialiseToCBORBytes v                  -- strict bytes
      hex  = B16.encode cbor
      json = LBS.fromStrict $
               C8.concat
                 [ C8.pack "{\n  \"type\": \"PlutusScriptV2\",\n  \"description\": \"\",\n  \"cborHex\": \""
                 , hex
                 , C8.pack "\"\n}\n"
                 ]
  LBS.writeFile fp json
```

**Result:** The output file is directly consumable by `cardano-cli`.


## 7) 🧰 Helper: Hex → PubKeyHash

```haskell
pkhFromHexUnsafe :: String -> PubKeyHash
pkhFromHexUnsafe hex =
  case B16.decode (C.pack hex) of
    Right raw -> PubKeyHash (Builtins.toBuiltin raw)
    Left  _   -> error "pkhFromHexUnsafe: invalid hex"
```

> **Unsafe:** This throws on invalid input. For production, prefer a safe variant returning `Either String PubKeyHash`.


## 8) 🏗️ Build & Run with Cabal

**Executable stanza (example)**

```cabal
executable plutus
  hs-source-dirs:     lecture/boilerPlate
  main-is:            UniversalTestLockUnlock.hs
  build-depends:
      base >=4.14 && <4.15
    , plutus-ledger-api
    , plutus-tx
    , bytestring
    , serialise
    , base16-bytestring
  default-language:   Haskell2010
  ghc-options:        -Wall -O2
```

**Steps**

1. Create the output folder: `mkdir -p ./assets`
2. Build: `cabal build plutus`
3. Run: `cabal run plutus`

You should see `./assets/lock-by-deadline.plutus` created with a JSON text envelope.


## 9) 🔍 Validate the Output

Open the generated file — it should look like:

```json
{
  "type": "PlutusScriptV2",
  "description": "",
  "cborHex": "<hex bytes...>"
}
```

Use `cardano-cli` commands to derive the script hash and address, and fund it with a UTxO for testing.


## 10) 🧯 Troubleshooting & Pitfalls

* **Deadline units:** Plutus `POSIXTime` is *milliseconds*. If you pass seconds, your rule will unlock ~1000× earlier than intended.
* **NoImplicitPrelude:** Import `Either(..)`, `IO`, `FilePath`, operators like `*`, `$` explicitly.
* **Text envelope vs raw CBOR:** Cardano CLI expects the JSON text envelope, not bare CBOR.
* **Beneficiary key:** Ensure the signer actually holds the payment key that corresponds to the `PubKeyHash` you use.
* **Clock skew:** Give a small buffer on the validity interval to account for slot timing.


## 11) 📚 Glossary

* **Validator:** On-chain program controlling spending from a script address.
* **Params:** Serializable configuration captured into the script at compile time.
* **Rule:** Declarative condition checked against `ScriptContext`.
* **Text Envelope:** JSON wrapper (`type`, `description`, `cborHex`) used by `cardano-cli`.
* **POSIXTime:** Milliseconds since epoch in Plutus.



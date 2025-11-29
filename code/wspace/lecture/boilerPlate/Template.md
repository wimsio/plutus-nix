# 🧩 Universal Plutus Validator Template — A Complete Guide

## 📑 Table of Contents

1. 🔭 Overview
2. 🧱 Core Concepts (Params, Rule, Specs)
3. 🧠 Design Goals & Guarantees
4. 🛠️ Module Walkthrough (numbered, with code)
5. 🧪 Quick Start: Lock-by-Deadline & Beneficiary
6. 🧰 Collectors: Reusing Datums/Redeemers (0..n)
7. 🧮 Writing Rules: What’s Included & How to Extend
8. 🧵 Wiring: Compiling to a Validator / Policy
9. 🧾 Serialization: Raw CBOR & Text Envelope
10. 🧯 Troubleshooting & Pitfalls
11. 🧭 Version Notes (Plutus V2 + Template Haskell)
12. 📚 Glossary of Terms

## 1) 🔭 Overview

**What it is:** A universal, reusable **rules engine** for Plutus V2.
**What it does:** Turns a **bag of declarative checks** (e.g., “signed by Alice”, “after deadline”, “no mint”) into a **validator** or **minting policy**.

**Why it helps:**

* You can **plug different rules** together without re-writing low-level script logic.
* Supports **0..n parameters**, **0..n datums**, **0..n redeemers**, and **always** requires a `ScriptContext`.
* Works cleanly with **NoImplicitPrelude** and your **V2 Ledger** stack.


## 2) 🧱 Core Concepts

### 2.1 `Cmp` (comparison over integers)

```haskell
data Cmp = EQI Integer | GE Integer | LE Integer | BETWEEN Integer Integer
```

Used by rules that compare on-chain integer amounts (e.g., minted amounts).

### 2.2 `Rule`

```haskell
data Rule
  = RequireAnySigner [PubKeyHash]
  | ValidAfter POSIXTime
  | ValidWithin POSIXTime POSIXTime
  | PreserveValueOneContinuing
  | NoMint
  | SameScriptContinuing
  | MintAmount CurrencySymbol TokenName Cmp   -- (stubbed in the baseline)
  | Pass
```

Each rule is **serializable** (via `makeIsDataIndexed`) and **pure on-chain**.

### 2.3 Specs & Params

```haskell
newtype SpendingSpec = SpendingSpec { spendingRules :: [Rule] }
newtype MintingSpec  = MintingSpec  { mintingRules  :: [Rule] }

data Params = Params
  { sSpec :: SpendingSpec
  , mSpec :: MintingSpec
  , extra :: [BuiltinData]   -- 0..n “param datums” you can decode as needed
  }
```


## 3) 🧠 Design Goals & Guarantees

* **Declarative**: Express behavior as data (`Rule`) — easy to audit, test, and reuse.
* **Universal**: Works for **any** spending or minting use case by composing rules.
* **Expandable**: Add your own rules without touching the engine.
* **Stable Imports**: Mirrors your working vesting modules; avoids version traps (e.g., `flattenValue` differences).
* **No Hidden Off-chain Logic**: All real checks live in on-chain code.


## 4) 🛠️ Module Walkthrough

### 4.1 Type-level Plumbing & TH Derives ✅

* Use `PlutusTx.makeIsDataIndexed` for **on-chain data** serialization.
* Use `PlutusTx.makeLift` so `Params` (and friends) can be **embedded** with `liftCode`.

```haskell
PlutusTx.makeIsDataIndexed ''Cmp   [ ('EQI,0),('GE,1),('LE,2),('BETWEEN,3) ]
PlutusTx.makeLift ''Cmp
-- … same for Rule, SpendingSpec, MintingSpec, Params
```

> **Why:** `liftCode p` (where `p :: Params`) needs `Lift` instances.

### 4.2 Small Stdlib Helpers 🧰

```haskell
{-# INLINABLE allChecks #-}
allChecks :: [Bool] -> Bool
allChecks = foldr (&&) True

{-# INLINABLE anyP #-}
anyP :: (a -> Bool) -> [a] -> Bool
anyP p = foldr (\x acc -> p x || acc) False
```

### 4.3 Collectors: 0..n Datums/Redeemers 🧲

```haskell
collectDatumsOf    :: forall d. FromData d    => ScriptContext -> [d]
collectRedeemersOf :: forall r. FromData r    => ScriptContext -> [r]
```

* Implemented via `txInfoData` / `txInfoRedeemers` and `AssocMap.toList + fromBuiltinData`.
* Lets you **safely** decode multiple typed datums/redeemers.

### 4.4 Spending Helpers 🧷

```haskell
ownInput :: ScriptContext -> TxOut
continuingOutExactlyOne :: ScriptContext -> TxOut
```

* Encapsulate boilerplate for “find own input” and “exactly one continuing output”.

### 4.5 Evaluating Rules 🧮

```haskell
evalRule :: Rule -> ScriptContext -> Bool
```

Implements all built-in rules with clear failure traces:

* `RequireAnySigner` — checks `txSignedBy info`.
* `ValidAfter` / `ValidWithin` — checks the **valid range** via `contains`, `from`, and `interval`.
* `PreserveValueOneContinuing` — checks script value continuity.
* `NoMint` — checks `txInfoMint == mempty`.
* `SameScriptContinuing` — ensures continuing output uses the **same** script hash.
* `MintAmount` — **stubbed** in baseline; see §7.3 to implement for your version.
* `Pass` — always `True` (useful placeholder).

### 4.6 Applying Many Rules at Once 🧪

```haskell
validateRules :: [Rule] -> ScriptContext -> Bool
validateRules rules ctx = allChecks (map (\r -> evalRule r ctx) rules)
```

### 4.7 Wrappers: Build Validator / Policy 🧵

```haskell
mkSpendingValidator :: Params -> Validator
mkMintingPolicy     :: Params -> MintingPolicy
```

* **Inlines** a lambda that captures `Params` via `liftCode`.
* Ensures the **spending** path only runs `sSpec` rules, **policy** only runs `mSpec`.


## 5) 🧪 Quick Start: Lock-by-Deadline & Beneficiary

Goal: Spend only if **signed** by `beneficiary` **after** `deadline`.

```haskell
mkLockByDeadlineParams :: PubKeyHash -> POSIXTime -> Params
mkLockByDeadlineParams beneficiaryPKH deadline =
  let spendingRules =
        [ RequireAnySigner [beneficiaryPKH]
        , ValidAfter deadline
        ]
  in Params
      { sSpec = SpendingSpec spendingRules
      , mSpec = MintingSpec []   -- no mint rules
      , extra = []               -- free param slots if you need them
      }

mkLockByDeadlineValidator :: PubKeyHash -> POSIXTime -> Validator
mkLockByDeadlineValidator pkh dl =
  mkSpendingValidator (mkLockByDeadlineParams pkh dl)
```

**Notes:**

* `POSIXTime` in Plutus is **milliseconds** since epoch. Convert seconds → `* 1000`.
* `RequireAnySigner [pkh]` accepts **any** signer in the list; pass multiple PKHs to accept a team.


## 6) 🧰 Collectors: Reusing Datums/Redeemers (0..n)

When your contracts pass rich, typed data around, read them in bulk:

```haskell
{-# INLINABLE collectDatumsOf #-}
collectDatumsOf :: forall d. PlutusTx.FromData d => ScriptContext -> [d]
-- decode all datum bodies that successfully parse as `d`

{-# INLINABLE collectRedeemersOf #-}
collectRedeemersOf :: forall r. PlutusTx.FromData r => ScriptContext -> [r]
-- decode all redeemer bodies that successfully parse as `r`
```

**Use cases:**

* Multiple bids/orders as datums on inputs.
* Multiple action redeemers (e.g., `Bid`, `Cancel`, `Close`) present in a tx.
* Parametrization via `extra :: [BuiltinData]` in `Params`.


## 7) 🧮 Writing Rules

### 7.1 Ready-to-use Rules ✅

* **Signers**: `RequireAnySigner [PubKeyHash]`
* **Validity range**: `ValidAfter t`, `ValidWithin start delta`
* **Value continuity**: `PreserveValueOneContinuing`
* **Mint restrictions**: `NoMint`
* **Same script hash**: `SameScriptContinuing`
* **No-op**: `Pass`

### 7.2 Add Your Own Rule 🧱

1. Extend the `Rule` data type (add your constructor).
2. Extend the `makeIsDataIndexed` mapping.
3. Extend `evalRule` with your logic — keep `traceIfFalse` labels concise.

Example: require **exactly one** signer from a set:

```haskell
-- 1) data Rule = ... | RequireExactlyOneSigner [PubKeyHash]
-- 2) assign a new index in makeIsDataIndexed
-- 3) extend evalRule:
RequireExactlyOneSigner pks ->
  let sigs = txInfoSignatories info
      n    = length (filter (\pk -> anyP (== pk) sigs) pks)
  in traceIfFalse "ExactlyOneSigner" (n == 1)
```

### 7.3 Implementing `MintAmount` (when you’re ready) 🪙

Some Plutus snapshots differ in how to inspect `txInfoMint`. If `flattenValue` isn’t available in your environment, you can:

* use `Ledger.Value.valueOf` if present in your stack, or
* roll your own `AssocMap` walker (depends on your exact value representation).

Keep it **stubbed** until you need it; your spending examples (like lock-by-deadline) don’t depend on it.


## 8) 🧵 Wiring: Compiling Scripts

The template uses the **inline-lambda + `liftCode`** pattern:

```haskell
mkSpendingValidator p =
  let wrapped = $$(compile [||
        \p' _datum _redeemer ctx ->
          let ctx'  = unsafeFromBuiltinData ctx
              rules = unpackSpendingRules p'
          in if validateRules rules ctx' then () else traceError "validation failed"
        ||]) `applyCode` liftCode p
  in mkValidatorScript wrapped
```

**Why this pattern:** closures can’t cross the on-chain boundary, but **values** with `Lift` instances can (via `liftCode`). This captures your `Params` **at compile time** in the script.


## 9) 🧾 Serialization: Raw CBOR & Text Envelope

You already have two styles:

* **Raw CBOR** (lazy bytestring):

  ```haskell
  serialiseToCBOR :: Validator -> LBS.ByteString
  writeValidatorCBOR :: FilePath -> Validator -> IO ()
  ```

* **Cardano CLI text envelope**:

  ```json
  {
    "type": "PlutusScriptV2",
    "description": "",
    "cborHex": "<hex>"
  }
  ```

  Use your `writeValidatorEnvelope` helper to produce this JSON for `cardano-cli`.

**CLI tip:** If you need a *script address* to lock ADA:

* Derive it from the validator hash and network tag (testnet/mainnet) in your off-chain code or utilities module.


## 10) 🧯 Troubleshooting & Pitfalls

* **Duplicate instances / types**
  Ensure each `data` and each `makeIsDataIndexed/makeLift` runs **once** per type.

* **`deriveLift` vs `makeLift`**
  Your stack uses `makeLift`. Don’t import `deriveLift`.

* **`POSIXTime` units**
  Must be **milliseconds**. Convert seconds → `* 1000`.

* **`NoImplicitPrelude`**
  If you use `(.)`, `Right/Left`, `String`, `IO`, `FilePath`, you must import them from `Prelude` explicitly.

* **AssocMap vs list**
  Use `PlutusTx.AssocMap.toList` to iterate `txInfoData` / `txInfoRedeemers`.

* **`flattenValue` missing**
  Leave `MintAmount` stubbed until you implement a version compatible with your value representation.


## 11) 🧭 Version Notes (Plutus V2 + TH)

* GHC **8.10.7**
* `Plutus.V2.Ledger.Api`
* Template Haskell with **`makeIsDataIndexed`** and **`makeLift`**
* Context helpers from **`Plutus.V2.Ledger.Contexts`**

The import style mirrors your **Vesting** modules to avoid “does not export” errors across snapshots.


## 12) 📚 Glossary of Terms

* **Validator**: On-chain script that governs **spending** from a script address.
* **Minting Policy**: On-chain script that governs **minting/burning** of tokens.
* **Params**: The **configuration** captured into the script at compile time (rules + extra data).
* **Rule**: A **declarative check** (e.g., signer, time, continuing value) the engine evaluates.
* **Datum / Redeemer**: User-provided **data** attached to outputs (datum) or inputs/actions (redeemer).
* **ScriptContext**: Full transaction context available to the script at validation time.
* **Continuing Output**: Output that **remains at the same script** after spending (state transition).
* **AssocMap**: On-chain associative map type used in transaction info fields.
* **Lift**: Template Haskell mechanism to **embed Haskell values** inside compiled Plutus Core.
* **CBOR / Text Envelope**: Binary form of your script and a JSON wrapper recognized by `cardano-cli`.


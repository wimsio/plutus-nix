# Constant Product Automated Market Maker (AMM) DEX — A Complete Tutorial (Cardano/Plutus)

**Author:** Coxygen Global
**Date & Time:** Wednesday, November 12, 2025 at 07:50 (Africa/Johannesburg)
**License** MIT

## Table of Contents

1. 🔰 Introduction
2. 🧩 Core Concepts and Definitions
3. 🏗️ System Architecture (Cardano UTxO)
4. 🔄 Core User Flows
5. 🧮 Math & Worked Examples
6. 🧑‍⚖️ Validator Rules (Haskell/Plutus V2)
7. 🛡️ Security Requirements & Threat Model
8. 🚀 Scaling to a Uniswap-Class Competitor
9. 🧰 Operational Practices & Tooling
10. 📚 Glossary of Terms
11. 📎 Appendix: Prior Q&A (“You said…”)
12. 🧩 Reference Implementation (Compiling `AMM.hs`)

## 1. 🔰 Introduction

Automated Market Makers (AMMs) replace order books with **on-chain liquidity pools** governed by a simple pricing rule. This tutorial walks you through designing and building a **Constant-Product AMM (CPMM)** DEX on **Cardano** using **Plutus V2**, from core math to validator rules, production-grade security, and a roadmap to scale into a Uniswap-class exchange.

This guide is fully synchronized with a **compiling `AMM.hs`** that:

* Implements a **pool validator** (AddLiquidity, Swap, RemoveLiquidity)
* Implements an **LP mint/burn policy** guarded by the pool NFT + participation
* Uses a configurable **fee in basis points** (`Bps`), slippage guards, and multiple LPs
* **Exports `.plutus` files** (cardano-cli text envelopes) for both scripts

## 2. 🧩 Core Concepts and Definitions

**LP (Liquidity Provider)** — Deposits both assets into a pool and receives **LP tokens** representing fractional ownership of reserves and fees.

**CPMM (Constant-Product Market Maker)** — Keeps (x\cdot y \approx k), with fees making (k) grow. Spot price ≈ (y/x) for X in units of Y.

**TVL (Total Value Locked)** — Total value of all assets locked in a protocol/pool. Higher TVL ⇒ deeper liquidity ⇒ lower slippage.

**AMM (Automated Market Maker)** — Smart-contract exchange that quotes prices algorithmically (e.g., CPMM) instead of matching orders.

**Slippage** — Difference between expected output and actual execution, due to price impact and latency/MEV. Controlled with `minOut`/`maxIn`.

## 3. 🏗️ System Architecture (Cardano UTxO)

### 3.1 Pool Identity

* **Pool NFT** locked in the pool UTxO uniquely identifies the pool across transitions.
* **Pool Datum** stores reserves and (optionally) cached supply hints. In this didactic build:

  * Reserves: `dAdaR`, `dTokR`
  * Asset IDs are immutable in **`PoolParams`**: token `(CS,TN)`, LP `(CS,TN)`, pool NFT `(CS,TN)`
  * Fee rate `ppFeeBps :: Bps` (e.g., `Bps 100` = 1%)

### 3.2 Transaction Shape

* **Add Liquidity:** 1 pool input → 1 continuing output (NFT preserved), LP **mint** to depositor
* **Swap:** 1 pool input → 1 continuing output, **no LP mint/burn**
* **Remove Liquidity:** 1 pool input → 1 continuing output, LP **burn** from withdrawer

### 3.3 Reference Scripts & Inline Datums

* Prefer **reference scripts** for fee efficiency.
* Support both **`OutputDatum`** and **`OutputDatumHash`** (the validator handles either when decoding datums).

## 4. 🔄 Core User Flows

### 4.1 Pool Creation

1. Mint the **pool NFT**; 2) Create initial pool UTxO with datum `{dAdaR=0, dTokR=0}` and the NFT; 3) First liquidity **bootstraps** LP supply.

### 4.2 Add Liquidity

* Depositor provides ADA + Token **proportionally** to current reserves.
* Checks: proportionality, exact reserve deltas, **single** LP mint triple, NFT continuity, datum update.

### 4.3 Swap (ADA↔Token)

* Trader submits input asset and `minOut` (slippage tolerance).
* Checks: fee-adjusted input, CPMM output, exact reserve transitions, **no mint/burn**, NFT continuity.

### 4.4 Remove Liquidity

* LP burns `lpBurn`; receives pro-rata ADA and tokens (validator checks via deltas).
* Checks: exact **LP burn** triple, minimum payouts, reserves do not increase, NFT continuity.

## 5. 🧮 Math & Worked Examples

### 5.1 Invariant & Price

Without fee: ((x+\Delta x)(y-\Delta y)=xy\Rightarrow \Delta y=\dfrac{\Delta x,y}{x+\Delta x}).
With fee (\gamma): (\Delta x_\text{eff}=\Delta x(1-\gamma)) — use (\Delta x_\text{eff}) in the formula.

### 5.2 LP Minting (existing pool)

Given reserves `(x,y)`, total LP `L`, deposit `(Δx,Δy)` proportional:
[ \text{LP}_\text{minted} = L\cdot\min(\tfrac{\Delta x}{x},\tfrac{\Delta y}{y}) ]
If perfectly proportional, `LP_minted = L*(Δx/x)`.

**Example:** If `x` receives 10 and you add matching `Δy = y*(10/x)`, then `LP_minted = L*(10/x)`.

### 5.3 Bootstrap LP (empty pool)

Common rule: (\text{LP}_\text{minted} = \lfloor\sqrt{\Delta x\cdot\Delta y}\rfloor) (optionally scaled), less a small locked minimum.

### 5.4 Slippage

`slippage% = (expected − actual)/expected × 100%`. Guard with `minOut` or `maxIn`.


## 6. 🧑‍⚖️ Validator Rules (Haskell/Plutus V2)

### 6.1 Types & Instances (on-chain ready)

* **`newtype Bps = Bps Integer`**

  * `PlutusTx.unstableMakeIsData ''Bps`
  * `PlutusTx.makeLift ''Bps`
* **`PoolParams`** — immutable asset IDs and fee bps:

  * `PlutusTx.unstableMakeIsData ''PoolParams`
  * `PlutusTx.makeLift ''PoolParams`
* **`PoolDatum { dAdaR, dTokR }`** and **`Action`** (`AddLiquidity | Swap | RemoveLiquidity`) both derive `IsData`.
* **Tuple equality** for `flattenValue` checks uses `{-# LANGUAGE FlexibleInstances #-}` and a small `Eq (CurrencySymbol, TokenName, Integer)` instance.

### 6.2 Helper Primitives

* `adaOf`, `tokOf`, `lpOf`, `nftOf` for asset extraction; `findOwnInput'` (strict) and `decodeDatum` for datum loading.
* Fees & CPMM:
  `feeEff (Bps bps) x = divide (x * (10000 - bps)) 10000`
  `cpmmOut x y dxEff = if dxEff <= 0 then 0 else divide (dxEff * y) (x + dxEff)`
* `ensure :: Bool -> ()` convenience checker.

### 6.3 Pool Validator

```haskell
mkPoolValidator :: PoolParams -> PoolDatum -> Action -> ScriptContext -> Bool
```

**Common gates:**

1. Exactly one continuing output; 2) **Pool NFT** present on input and output; 3) Decode pre/post datums; 4) (Optional) forbid foreign assets via `flattenValue` scan.

**AddLiquidity dAda dTok minLP**

* Bootstrap allowed when `adaR == 0 && tokR == 0` with `lpMinted = ⌊√(dAda*dTok)⌋`.
* Otherwise enforce proportionality: `dAda*tokR == dTok*adaR`.
* Reserves update exactly; **single** LP mint triple:
  `flattenValue minted == [(ppLpCS, ppLpTN, lpMinted)]` and `lpOf pp minted == lpMinted`.

**Swap dir amount minOut**

* `dir=True` ADA→Token; `False` Token→ADA.
* Apply `feeEff` to input then CPMM; enforce reserves update and `dy ≥ minOut`; **no mint/burn**.

**RemoveLiquidity lpBurn minAdaOut minTokOut**

* Require exact LP burn triple:
  `flattenValue minted == [(ppLpCS, ppLpTN, negate lpBurn)]`.
* Compute deltas: `dAdaOut = adaR - adaR'`, `dTokOut = tokR - tokR'`; enforce `dAdaOut ≥ minAdaOut && dTokOut ≥ minTokOut` and reserves not increasing.

### 6.4 LP Minting Policy (no external deps)

```haskell
mkLpPolicy :: PoolParams -> () -> ScriptContext -> Bool
```

* Valid iff the transaction **spends a pool input** carrying the **pool NFT**.
* **Untyped wrapper** using manual decoding (no `plutus-script-utils` required):

```haskell
wrappedMkLpPolicy :: PoolParams -> BuiltinData -> BuiltinData -> ()
wrappedMkLpPolicy pp d1 d2 =
  let r   = unsafeFromBuiltinData @() d1
      ctx = unsafeFromBuiltinData @ScriptContext d2
  in if mkLpPolicy pp r ctx then () else traceError "mkLpPolicy failed"
```

* Compiled with Template Haskell and `liftCode pp` into a `MintingPolicy`.

### 6.5 Integer Rounding & Safety

* Mint path rounds **down**; swap uses safe integer division; burn path checks exact deltas and negative mint quantity.


## 7. 🛡️ Security Requirements & Threat Model

### 7.1 Identity & State

* Pool NFT binds state; verify token IDs `(CS,TN)` and ADA `(adaSymbol, adaToken)` precisely.
* All state from **datum**; redeemer carries only action/minimums/params.

### 7.2 Mint/Burn Discipline

* Add → **mint only**; Remove → **burn only**; Swap → **no mint/burn**. Forbid other assets in `txInfoMint`.

### 7.3 Value Accounting

* Enforce exact reserve transitions; allow only min-ADA dust; otherwise forbid foreign assets in the pool UTxO.

### 7.4 Slippage & MEV

* `minOut` / `maxIn` required. Optional: batch auctions, time-bounded quotes, private mempools.

### 7.5 Access Controls (optional)

* Governance key for fee switch/params; circuit breaker on abnormal deltas.

### 7.6 Audits & Testing

* Property tests: fee makes `k` non-decreasing; no unintended mint; NFT continuity.
* Fuzz rounding edges; mutation tests; static analysis; manual reviews.

## 8. 🚀 Scaling to a Uniswap-Class Competitor

* **Fee tiers** (0.05%, 0.30%, 1.00%).
* **Stableswap curves** for correlated assets.
* **Concentrated liquidity** (range orders; positions as NFTs).
* **Routing**: multi-hop and split routes.
* **Oracles & Analytics**: TWAP price feeds; dashboards for TVL, volume, fees, IL.

## 9. 🧰 Operational Practices & Tooling

### 9.1 Building

```bash
nix-shell
cabal build
```

### 9.2 Exporting `.plutus` Scripts (already in `AMM.hs`)

* **Serializers** for `Validator` and `MintingPolicy` produce **cardano-cli text envelopes**:

```haskell
writeValidatorEnvelope :: FilePath -> Validator     -> IO ()
writePolicyEnvelope    :: FilePath -> MintingPolicy -> IO ()
```

* **Convenience entrypoint**:

```haskell
exportPlutusScripts :: IO ()
exportPlutusScripts = do
  let poolValidator = poolValidatorScript params
      lpPolicy      = lpMintingPolicy params
  writeValidatorEnvelope "./assets/pool-validator.plutus" poolValidator
  writePolicyEnvelope    "./assets/lp-policy.plutus"      lpPolicy
```

* **Outputs**:

  * `./assets/pool-validator.plutus`  (pool validator)
  * `./assets/lp-policy.plutus`       (LP mint/burn policy)

### 9.3 Example CLI Usage

```bash
cardano-cli transaction build \
  --tx-in ... \
  --tx-out ... \
  --mint-script-file ./assets/lp-policy.plutus \
  --out-file tx.raw
```

(Adjust flags per network and script usage.)

### 9.4 Emulator Seeds

* Demo IDs: `fakeCS1`, `fakeCS2`, `fakeCS3`; tokens: `tokenTN`, `lpTN`, `nftTN` and `params` with `Bps 100` (1%).


## 10. 📚 Glossary of Terms

* **AMM:** Automated Market Maker
* **CPMM:** Constant-Product MM ((x\cdot y = k))
* **LP:** Liquidity Provider; also the token representing pool shares
* **LP Tokens:** Fungible representation of pool ownership
* **TVL:** Total Value Locked
* **Slippage:** Deviation between expected and actual execution price/output
* **MinOut/MaxIn:** Slippage-protection guardrails
* **Pool NFT:** Unique token tying pool UTxO identity across transitions
* **Datum:** On-chain data payload attached to a UTxO
* **Reference Script:** Script stored on-chain to be referenced by future txs
* **TWAP:** Time-Weighted Average Price
* **CIP-68:** Standard for NFTs with on-chain metadata/state

## 11. 📎 Appendix: Prior Q&A (“You said…”)

* *What is LP? CPMM? TVL? AMM?* — See §2.
* *What is x · y = k?* — See §5.1.
* *What is slippage?* — See §2 and §5.4.
* *If x receives 10 how many LP tokens?* — See §5.2.
* *How does one decide on total LPs?* — Elastic supply & policy in §5.3 and §6.5.

## 12. 🧩 Reference Implementation (Compiling `AMM.hs`)

**Module:** `AMM`
**Key exports:**

* `poolValidatorScript :: PoolParams -> Validator`
* `lpMintingPolicy    :: PoolParams -> MintingPolicy`
* `exportPlutusScripts :: IO ()` — writes both `.plutus` text-envelopes
  **Params template:**

```haskell
params :: PoolParams
params = PoolParams
  { ppTokenCS   = "ff01"
  , ppTokenTN   = "TOK"
  , ppLpCS      = "ff02"
  , ppLpTN      = "LP"
  , ppPoolNftCS = "ff03"
  , ppPoolNftTN = "POOLNFT"
  , ppFeeBps    = Bps 100  -- 1%
  }
```

> The source above compiles with `ghc-8.10.7` in your setup and ships a minimal, production-style AMM ready for emulator traces and CLI usage.


# 🎯 Detailed Tutorial: Understanding and Using Milestone Escrow Crowdfunding Smart Contract

This tutorial covers a secure, milestone-based crowdfunding system with escrow protection on the Cardano blockchain.

---

## 📚 Table of Contents

1. [📦 Imports Overview](#1-imports-overview)
2. [🗃️ Data Structures](#2-data-structures)
3. [🧠 Core Validator Logic](#3-core-validator-logic)
4. [⚙️ Validator Script Compilation](#4-validator-script-compilation)
5. [🔧 Helper Functions](#5-helper-functions)
6. [🧪 Practical Usage Example](#6-practical-usage-example)
7. [🧷 Testing Strategy](#7-testing-strategy)
8. [✅ Best Practices](#8-best-practices)
9. [📘 Glossary of Terms](#9-glossary-of-terms)

---

## 1. 📦 Imports Overview

### Plutus Core Modules

* **Plutus.V2.Ledger.Api:**
  Provides fundamental types: `POSIXTime`, `PubKeyHash`, `BuiltinData`, `ScriptContext`.

* **Plutus.V2.Ledger.Contexts:**
  Contains utility functions for transaction context validation.

* **Plutus.V1.Ledger.Interval:**
  Supplies interval functionality for time-based campaign management.

* **Plutus.V1.Ledger.Value:**
  Provides asset value operations for fund tracking.

### Utility Modules

* **PlutusTx:**
  Enables script compilation and data serialization.

* **PlutusTx.Prelude:**
  Basic Plutus scripting functions.

* **Codec.Serialise:**
  Handles script serialization for deployment.

---

## 2. 🗃️ Data Structures

### `CampaignDatum` (Main Campaign State)

Defines the crowdfunding campaign state:

* `cdCreator`: Project creator's public key hash
* `cdTarget`: Funding target in Lovelace
* `cdDeadline`: Campaign deadline timestamp
* `cdRaised`: Total amount raised so far
* `cdMilestoneIdx`: Current milestone index

### `PledgeDatum` (Individual Backer State)

Defines individual backer contributions:
* `pdBacker`: Backer's public key hash
* `pdAmount`: Pledged amount in Lovelace

### `EscrowAction`

Defines possible campaign operations:
* `Pledge`: Backer contributes to campaign
* `UnlockMilestone`: Creator accesses funds after milestone
* `Refund`: Backer reclaims funds if campaign fails

---

## 3. 🧠 Core Validator Logic

### `mkValidator`

**Purpose:** Manages crowdfunding campaigns with milestone-based fund release and backer protection.

**Validation Conditions:**

#### Pledge (Contribute Funds):
* Current time must be before deadline
* Transaction must increase total raised amount
* Uses minted ADA to track pledge amounts

#### UnlockMilestone (Creator Access):
* Transaction signed by campaign creator
* Total raised must meet or exceed funding target
* Creator can access funds incrementally by milestones

#### Refund (Backer Protection):
* Current time must be after deadline
* Total raised must be below funding target
* Allows backers to reclaim their contributions

### Helper Functions

#### `afterDeadline`
Checks if current transaction time is after the campaign deadline.

#### `signedBy`
Verifies transaction signature by specific public key hash.

---

## 4. ⚙️ Validator Script Compilation

### `mkValidator`
Direct compilation of the validator logic.

### `validator`
Compiles the validator into a Plutus Core script ready for deployment.

---

## 5. 🔧 Helper Functions

### `writeValidator`
Serializes and writes the validator to a `.plutus` file for deployment.

---

## 6. 🧪 Practical Usage Example

```haskell
-- Generate and deploy the validator
main

-- Expected output:
-- Wrote validator to: milestone-escrow.plutus
-- --- Milestone Escrow Contract ---
-- Validator Hash: ValidatorHash...
-- Plutus Script Address: Address...
-- Bech32 Script Address: addr_test...
-- ---------------------------------
```

**Crowdfunding Operations:**

1. **Backer Pledge:**
   - Contribute funds during active campaign
   - Increase total raised amount
   - Secure funds in escrow

2. **Milestone Unlock:**
   - Creator access funds after target reached
   - Incremental release based on milestones
   - Requires creator authorization

3. **Backer Refund:**
   - Reclaim funds if campaign fails
   - Only available after deadline
   - Requires target not met condition

---

## 7. 🧷 Testing Strategy

* Test pledging during active campaign periods
* Verify milestone unlocking after target achievement
* Test refund functionality for failed campaigns
* Validate authorization requirements for all operations
* Test edge cases like exact target amounts

---

## 8. ✅ Best Practices

* Set realistic funding targets and deadlines
* Define clear milestone deliverables
* Implement transparent fund tracking
* Include comprehensive backer protection
* Test with various campaign success/failure scenarios

---

## 9. 📘 Glossary of Terms

| Term | Definition |
|------|-------------|
| **Escrow** | Neutral holding of funds until conditions met |
| **Milestone** | Project development phase with specific deliverables |
| **Pledge** | Backer commitment to contribute funds |
| **Funding Target** | Minimum amount required for project success |
| **Creator** | Project initiator seeking funding |

---
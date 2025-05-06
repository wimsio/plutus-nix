# 🧠 Plutus Smart Contracts Setup Guide with Nix & Cabal

<img src="https://github.com/user-attachments/assets/fe3da9b9-c2cf-4c2a-8ad6-b66e543e530a" alt="placeholder" width="80" height="50">
Pheidippides - rapid Plutus Application Development(rPAD)

Aim: To promote rapid, easy learning, simplification, and development on Cardano. Just like a greek messanger Pheidippides, who ran to deliver good news of victory, rPAD aims to quickly deliver quick dApps to Cardano communities.

Welcome to the Plutus development onboarding guide! This document will walk you through setting up a local Plutus development environment using **Nix** and **Cabal**, and understanding the `Vesting` contract structure, utilities, and key Haskell/Plutus concepts.

### NOTE! You will find detailed tutorials for modules in the following directory /code/wspace/lecture

---

## 📚 Table of Contents

1. [🧰 Prerequisites](#1-🧰-prerequisites)  
2. [⚙️ Environment Setup](#2-⚙️-environment-setup)  
3. [📦 Building the Project](#3-📦-building-the-project)  
4. [📁 Folder Structure](#4-📁-folder-structure)  
5. [🔍 Understanding the Contracts](#5-🔍-understanding-the-contracts)  
   - 5.1 [Basic Vesting](#51-basic-vesting)  
   - 5.2 [Parameterized Vesting](#52-parameterized-vesting)  
6. [🔧 Utilities Breakdown](#6-🔧-utilities-breakdown)  
7. [🧪 Testing and Debugging](#7-🧪-testing-and-debugging)  
8. [📖 Glossary of Terms](#8-📖-glossary-of-terms)  
9. [📝 License and Contributions](#9-📝-license-and-contributions)  

---

## 1. 🧰 Prerequisites

- [Nix](https://nixos.org/download.html)  
- Git CLI  
- Optional: VSCode with [Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)

---

## 2. ⚙️ Environment Setup

### a. Clone and Enter

```bash
git clone <your-repo-url>
cd plutus-nix
```

### b. Enter the Dev Shell

```bash
nix develop
```

If you're not using flakes, you can run:

```bash
nix-shell
```

---

## 3. 📦 Building the Project

```bash
cabal update
cabal build all
```

This will build both the **`Utilities`** library and the **`wspace`** smart contract/test modules.

---

## 4. 📁 Folder Structure

```text
plutus-nix/
├── .devcontainer/
├── .vscode/
├── code/
│   ├── dist-newstyle/
│   ├── nix/
│   ├── Utilities/
│   │   ├── src/
│   │   │   └── Utilities/
│   │   │       ├── Conversions.hs
│   │   │       ├── PlutusTx.hs
│   │   │       ├── Serialise.hs
│   │   │       └── Utilities.hs
│   │   ├── Utilities.cabal
│   │   └── hie.yaml
│   ├── wspace/
│   │   ├── assets/
│   │   ├── lecture/
│   │   │   ├── CGPlutusUtilsv1.hs
│   │   │   ├── CGTime.hs
│   │   │   ├── ParameterizedVesting.hs
│   │   │   └── Vesting.hs
│   │   ├── test/
│   │   │   ├── CGPlutusUtilsSpec.hs
│   │   │   ├── CGTimeSpec.hs
│   │   │   ├── VestingSpec.hs
│   │   │   ├── ParameterizedVestingSpec.hs
│   │   │   ├── Spec.hs
│   │   │   └── Main.hs
│   │   ├── docs/
│   │   ├── Tutorials.md
│   │   ├── cabal.project
│   │   └── wspace.cabal
├── .gitignore
├── flake.nix
└── README.md
```

---

## 5. 🔍 Understanding the Contracts

### 5.1 Basic Vesting

- **File**: `lecture/Vesting.hs`  
- Validates that:
  - A transaction is signed by the **beneficiary**
  - The **deadline** has been reached

### 5.2 Parameterized Vesting

- **File**: `lecture/ParameterizedVesting.hs`  
- Accepts:
  - `beneficiary :: PubKeyHash`
  - `deadline :: POSIXTime`  
- Uses `liftCode` to embed these at compile time

---

## 6. 🔧 Utilities Breakdown

### Address Utilities

- **File**: `lecture/CGPlutusUtilsv1.hs`  
  - Decode Bech32 → PubKeyHash  
  - Encode PubKeyHash → Bech32 (mainnet/testnet)

### Time Utilities

- **File**: `lecture/CGTime.hs`  
  - POSIX, ISO8601, UTC conversions  
  - Time arithmetic: add/diff/getNow

---

## 7. 🧪 Testing and Debugging

### Test Entry

```haskell
main :: IO ()
main = defaultMain tests
```

### Test Files

```text
test/
├── CGPlutusUtilsSpec.hs
├── CGTimeSpec.hs
├── VestingSpec.hs
├── ParameterizedVestingSpec.hs
├── Spec.hs
└── Main.hs
```

Run tests via:

```bash
cabal test all
```

---

## 8. 📖 Glossary of Terms

| Term                     | Description |
|--------------------------|-------------|
| **POSIXTime**            | Seconds since the Unix epoch |
| **PubKeyHash (PKH)**     | Hash of a wallet's public key |
| **Validator**            | The on-chain logic for validation |
| **ScriptContext**        | Transaction context during validation |
| **liftCode / applyCode** | Embeds values directly into compiled code |
| **Bech32**               | Human-readable address format for Cardano |
| **txSignedBy**           | Checks if a transaction is signed by a specific PKH |
| **Utilities Library**    | Helper functions for off-chain dev/test |
| **Cabal / Nix**          | Build and environment tools for Haskell & Plutus |

### 🆕 Additional Glossary Terms

| Term               | Description |
|--------------------|-------------|
| **Bech32**         | A human-readable encoding for addresses. |
| **PubKeyHash**     | A 28-byte hash of a public key used to identify wallets. |
| **POSIXTime**      | Seconds since Unix epoch (1970‑01‑01 UTC). |
| **UTCTime**        | Coordinated Universal Time representation in Haskell. |
| **HRP**            | Human‑Readable Part of a Bech32 string, indicates network. |
| **GADT**           | Generalized Algebraic Data Type, a Haskell feature for precise typing. |
| **Datum**          | On-chain data attached to UTxOs. |
| **Validator**      | A script that checks whether a transaction is allowed. |
| **On-chain**       | Code that runs in the blockchain’s validation. |
| **Off-chain**      | Code that runs in a user’s wallet or backend. |
| **CGTime**         | Coxygen Global Time module. |
| **CGPlutusUtils**  | Coxygen Global Plutus Utils module. |

## 9. 📝 License and Contributions

### License

```
MIT License

Copyright (c) 2025 Women In Move Solutions (Pty) Ltd

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

```

### Author & Ownership

- **Author:** Bernard Sibanda  
- **Company:** Coxygen Global  
- **Date:** May 05, 2025

Whos is Bernard Sibanda?

## **Professional Introduction: Bernard Sibanda**

I am **Bernard Sibanda**, a technologist, educator, and blockchain advocate with a diverse academic and technical background. I hold a **BSc in Computing**, a **BTech in Education**, and a **Diploma in Philosophy**, complemented by certifications including **Microsoft Certified Professional (MCP)**, **CompTIA A+**, and **Haskell Plutus** from the **European Business University of Luxembourg (EBU)**. I have **upskilled on the professional AI Engineer Path via Scrimba**, reflecting my deep commitment to continuous learning and innovation at the intersection of education, blockchain, and artificial intelligence.

I serve as the **Founder**, **Chief Technology Officer (CTO)**, and **Intersect Developer Advocate** at:

* **WIMS-Cardano Global**
* **Coxygen Global**
* **Satoshi Africa**
* **Tobb Technologies**

## **My Blockchain Focus**

My primary focus is on strengthening and scaling the **Cardano blockchain ecosystem**, working closely with **Intersect MBO** and **Project Catalyst**. I actively promote tools, strategies, and learning pathways that enhance **usability**, **utility**, and **developer onboarding** across Cardano.

### **Key Focus Areas**

* Empowering developer productivity with **Haskell/Plutus**, **Plutus Core**, and **Plinth**
* **Training alternative smart contract languages**: **Helios**, **PHP**, **Rust**, **Aiken**, and **Lucid**—across the world
* Contributing to **Plutus HA** development and formal verification methods
* Supporting the broader Cardano roadmap through inclusive, community-led innovation

## **My Aims and Objectives**

My mission is to **democratize access to smart contract development** by building an ecosystem that is accessible, fast, and developer-friendly on a global scale. I aim to:

* Deliver **live technical training and support** for Cardano smart contract development
* **Recruit thousands of university and college students** worldwide into the Cardano ecosystem
* **Onboard new developers** with practical, language-inclusive training approaches
* Build and maintain open-source libraries, templates, and documentation such as:

  * `coxylib.js` – a utility library for Cardano developers
  * `jimba.js` – tools for interactive learning and prototyping
  * `pheidippides (rPAD)` – a toolkit for **rapid Plutus App Development**
* Promote **fast learning, easy onboarding, and streamlined development workflows**
* Apply **formal methods** and **property-based testing** to ensure secure and reliable smart contracts
* **Collaborate with non-technical stakeholders**—from business leaders to promoters—to align technology with real-world needs
* Continuously **enhance the Cardano developer experience** by identifying friction points and developing effective solutions

I envision a world where **every developer can contribute to and benefit from Cardano—one of the top 10 blockchains in the world**.

---


# Changelog

## [1.0.3] - 2025-11-09

### 🚀 Overview

This is a plinth/plutus-tx-template designed to help plutus students and young developers get started developing Cardano Smart Contract quickly and easily.

Each plutus haskell module has tutorials, compiles and run with cabal repl, making it easy to get started. After running cabal update, cabal build the size should be around 700mb.

**Contributors:** Bernard Sibanda  

### ✨ Added

index.html :- Haskell Plutus Smart Contract Tutorial Playground
This displays Smart Contract Examples, Tutorials and demo frontends

Added tutorials for all haskell plutus modules so that each code snippet is explained. 

### 🧩 Fixed

Bugs due to compiling and building of Vesting, ParameterixedVesting.

### 🔧 Changed

Lecture directory now has boilerPlate with modules for flexible template. 
Added another directory validators with .js and .html files for plutus source code and frontend demos and tutorials.

### 🔧 Removed

Demo.hs
DemoSpec.hs
Mint.hs
MintSpec.hs
Escrow.hs
EscrowSpec.hs
Donation.hs   
DonationSpec.hs

### 👥 Contributors

- **Bernard Sibanda** 

### 🔧 Tests

All wspace Tests
  Vesting Module Tests
    Validator rejects before deadline:           OK
    Validator accepts after deadline:            OK
  Parameterized Vesting Tests
    fromHexPKH parses valid hex:                 OK
  CGTime Tests
    ISO8601 round-trip:                          OK
  CGPlutusUtils Tests
    Bech32 round-trip test:                      OK
  TemplateHaskellDemo Tests
    greet generates a greeting message:          Hello Bernard Sibanda OK
    add correctly sums two compile-time numbers: OK
    volFunction correctly computes volume:       OK
    gradeFunc returns correct grades:            OK
    triple cubes numbers correctly:              OK


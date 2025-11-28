> **Goal:** Learn to load, inspect, and test your Plutus `Vesting.hs` module directly inside GHCi — exploring every type, conversion, and value that composes your `VestingDatum`.

## 🧭 Table of Contents

1. [🎯 Goal & Prerequisites](#1-goal--prerequisites)
2. [⚙️ Project Setup & `cabal repl`](#2-project-setup--cabal-repl)
3. [🧪 GHCi Quick Primer](#3-ghci-quick-primer)
4. [📦 What Each Import Gives You (Expanded)](#4-what-each-import-gives-you-expanded)
5. [🧰 Working with Bytes — From String → ByteString → BuiltinByteString](#5-working-with-bytes--from-string--bytestring--builtinbytestring)
6. [🔤 Keys & Time — `PubKeyHash` and `POSIXTime`](#6-keys--time--pubkeyhash-and-posixtime)
7. [🏗️ Building a `VestingDatum` Value](#7-building-a-vestingdatum-value)
8. [⚡ One-Liners & REPL Tricks](#8-one-liners--repl-tricks)
9. [🔍 Type and Info Commands (`:t`, `:i`)](#9-type-and-info-commands-t-i)
10. [🧯 Common Pitfalls & Fixes](#10-common-pitfalls--fixes)
11. [📚 Glossary](#11-glossary)

## 1️⃣ 🎯 Goal & Prerequisites

You’ll learn how to:

* Start GHCi using `cabal repl`
* Import Plutus modules interactively
* Convert between text, bytes, and Plutus types
* Construct and inspect a full `VestingDatum` record

**You need:**

* A Plutus project (like `wspace`) with `Vesting.hs` containing:

  ```haskell
  data VestingDatum = VestingDatum
    { beneficiary :: PubKeyHash
    , deadline    :: POSIXTime
    , code        :: Integer
    } deriving (Show)
  ```
* GHC 8.10.7
* Plutus libraries installed

## 2️⃣ ⚙️ Project Setup & `cabal repl`

Start your REPL from the project root:

```bash
cabal repl
```

Output should look like:

```
GHCi, version 8.10.7...
Ok, six modules loaded.
```

Optional session settings:

```ghci
:set prompt "> "
:set -XOverloadedStrings
```

> `OverloadedStrings` makes `"abc"` polymorphic — can act as `String`, `Text`, or even `BuiltinByteString` depending on context.

## 3️⃣ 🧪 GHCi Quick Primer

| Command                       | Description                                        |
| ----------------------------- | -------------------------------------------------- |
| `:load Module` or `:l Module` | Load or reload a file                              |
| `:r`                          | Reload current modules                             |
| `:show modules`               | List all loaded modules                            |
| `:t expr`                     | Show type of an expression                         |
| `:i Name`                     | Show detailed info (type, constructors, instances) |
| `:set -XExtension`            | Enable a language extension                        |
| `:q`                          | Quit GHCi                                          |

> 🧠 *Pro tip:* GHCi prints every expression by doing `print it`. That’s why you need `deriving (Show)` to see values.

## 4️⃣ 📦 What Each Import Gives You (Expanded)

Let’s deeply understand each import that powers your vesting interaction.

---

### 🧩 `Data.ByteString.Char8`

**Purpose:** Convert between human-readable text and raw bytes (`ByteString`).
**Why:** Plutus and Base16 operate on `ByteString`, not `[Char]`.

| Function   | Type                   | Description                                              |
| ---------- | ---------------------- | -------------------------------------------------------- |
| `C.pack`   | `String -> ByteString` | Converts `"Hello"` to bytes `[72,101,108,108,111]`.      |
| `C.unpack` | `ByteString -> String` | Reverses `pack`, decoding bytes back into readable text. |

**Example:**

```haskell
import qualified Data.ByteString.Char8 as C
let rawBytes = C.pack "Bernard"
C.unpack rawBytes
-- "Bernard"
```

🔎 *Why Char8?*
It encodes each `Char` as one byte (ASCII), ideal for deterministic blockchain data.

### 🧮 `Data.ByteString.Base16`

**Purpose:** Encode/decode hexadecimal text to/from `ByteString`.
**Why:** Cardano addresses and pubkey hashes are usually in hex form.

| Function     | Type                                                | Description                                                                                               |
| ------------ | --------------------------------------------------- | --------------------------------------------------------------------------------------------------------- |
| `B16.decode` | `ByteString -> Either (ByteString, Int) ByteString` | Converts a hex string (like `"deadbeef"`) to binary bytes. Returns `Right` on success, `Left` on failure. |

**Example:**

```haskell
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
let (Right raw) = B16.decode (C.pack "096f2271b1b6dc76aa5ab7676d364d25...")
```

**Why Base16?**
`B16.decode` transforms a visible hash into its true underlying byte pattern for on-chain use.

### 🔗 `PlutusTx.Builtins`

**Purpose:** Bridge normal Haskell `ByteString` and Plutus `BuiltinByteString`.

| Name                | Type                              | Description                                                 |
| ------------------- | --------------------------------- | ----------------------------------------------------------- |
| `toBuiltin`         | `ByteString -> BuiltinByteString` | Converts standard bytes into the Plutus built-in type.      |
| `fromBuiltin`       | `BuiltinByteString -> ByteString` | Converts built-in bytes back to normal bytes for debugging. |
| `BuiltinByteString` | *type*                            | The native byte array type used inside Plutus scripts.      |

**Example:**

```haskell
import PlutusTx.Builtins (toBuiltin, fromBuiltin)
let plutusBytes = toBuiltin raw
C.unpack (fromBuiltin plutusBytes)
```

🧠 *Why convert?*
On-chain scripts use `BuiltinByteString` for deterministic operations — no lazy I/O, no hidden state.

### 🪙 `Plutus.V2.Ledger.Api`

Defines all the ledger-level Plutus data structures.
For this tutorial, you only need **two** newtypes: `PubKeyHash` and `POSIXTime`.

#### 🔐 `newtype PubKeyHash = PubKeyHash BuiltinByteString`

* Represents a **wallet’s payment key hash**.
* Wraps a `BuiltinByteString` for **type safety**.
* Required for `VestingDatum.beneficiary`.

**Usage:**

```haskell
import Plutus.V2.Ledger.Api (PubKeyHash(..))
let pkh = PubKeyHash plutusBytes
```

Check its constructor:

```haskell
:t PubKeyHash
-- PubKeyHash :: BuiltinByteString -> PubKeyHash
```

#### 🕒 `newtype POSIXTime = POSIXTime Integer`

* Represents a time on the blockchain (milliseconds since Unix epoch).
* Used in deadlines, validity intervals, etc.

**Usage:**

```haskell
import Plutus.V2.Ledger.Api (POSIXTime(..))
let t = POSIXTime 1760897124
```

Check its type:

```haskell
:t POSIXTime
-- POSIXTime :: Integer -> POSIXTime
```

### ⚙️ The Big Picture: Conversion Flow

```
"096f22..." (String)
   │  C.pack
   ▼
ByteString
   │  B16.decode
   ▼
Raw Bytes (ByteString)
   │  toBuiltin
   ▼
BuiltinByteString
   │  PubKeyHash
   ▼
PubKeyHash
```

Each layer wraps the previous one, ensuring data integrity and Plutus compatibility.

## 5️⃣ 🧰 Working with Bytes — From String → ByteString → BuiltinByteString

```haskell
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import PlutusTx.Builtins (toBuiltin)

let (Right raw) = B16.decode (C.pack "096f22...be1d")
let bsBuiltin   = toBuiltin raw
```

**Verify:**

```haskell
:t bsBuiltin
-- bsBuiltin :: BuiltinByteString
```

## 6️⃣ 🔤 Keys & Time — `PubKeyHash` and `POSIXTime`

```haskell
import Plutus.V2.Ledger.Api (PubKeyHash(..), POSIXTime(..))

let pkh = PubKeyHash bsBuiltin
let deadline = POSIXTime 1760897124
```

## 7️⃣ 🏗️ Building a `VestingDatum` Value

```haskell
import Vesting

let vd = VestingDatum
            { beneficiary = pkh
            , deadline    = deadline
            , code        = 58443
            }
```

If `VestingDatum` derives `Show`:

```haskell
vd
```

Prints:

```
VestingDatum {beneficiary = 096f22..., deadline = POSIXTime {getPOSIXTime = 1760897124}, code = 58443}
```

## 8️⃣ ⚡ One-Liners & REPL Tricks

**Compact one-liner:**

```haskell
let (Right raw)=B16.decode(C.pack"096f22...be1d");let vd=VestingDatum{beneficiary=PubKeyHash(toBuiltin raw),deadline=POSIXTime 1760897124,code=58443}
```

**Reload after edits:**

```ghci
:r
```

**View module exports:**

```ghci
:browse Plutus.V2.Ledger.Api
```

## 9️⃣ 🔍 Type and Info Commands (`:t`, `:i`)

| Command         | Example                | Result                        |
| --------------- | ---------------------- | ----------------------------- |
| `:t expr`       | `:t PubKeyHash`        | shows function signature      |
| `:i Type`       | `:i BuiltinByteString` | shows constructors, instances |
| `:show modules` | –                      | lists all modules loaded      |

## 🔟 🧯 Common Pitfalls & Fixes

| Issue                                     | Cause                                             | Fix                                                      |
| ----------------------------------------- | ------------------------------------------------- | -------------------------------------------------------- |
| `"abc" :: BuiltinByteString` fails        | GHCi string literals are `[Char]`                 | Use `toBuiltin (C.pack "abc")`                           |
| `No instance for (Show VestingDatum)`     | GHCi can’t print custom types                     | Add `deriving (Show)`                                    |
| `Couldn't match expected type PubKeyHash` | You passed raw bytes                              | Use `PubKeyHash (toBuiltin raw)`                         |
| `Base16 expects ByteString`               | Gave it `[Char]`                                  | Wrap with `C.pack`                                       |
| Parse error using `=`                     | Haskell record syntax uses `{ field = val, ... }` | Correct to `VestingDatum { field1 = ..., field2 = ... }` |

## 11️⃣ 📚 Glossary

| Term                        | Meaning                                        |
| --------------------------- | ---------------------------------------------- |
| **ByteString**              | Efficient raw byte array for binary data.      |
| **BuiltinByteString**       | Plutus built-in byte array used on-chain.      |
| **toBuiltin / fromBuiltin** | Convert between Haskell and Plutus bytes.      |
| **Base16 (Hex)**            | Encodes binary data as readable hexadecimal.   |
| **PubKeyHash**              | Wallet/public-key identifier in Plutus.        |
| **POSIXTime**               | Blockchain-safe timestamp wrapper.             |
| **VestingDatum**            | On-chain data type specifying vesting details. |
| **`:t` / `:i`**             | Show type / show info in GHCi.                 |
| **OverloadedStrings**       | Extension for polymorphic string literals.     |

### ✅ Final Working GHCi Script

```haskell
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base16 as B16
import PlutusTx.Builtins (toBuiltin)
import Plutus.V2.Ledger.Api (PubKeyHash(..), POSIXTime(..))
import Vesting

let (Right raw) = B16.decode (C.pack "096f2271b1b6dc76aa5ab7676d364d254aa484bbd5f6f7f356bcbe1d")
let pkh = PubKeyHash (toBuiltin raw)
let vd  = VestingDatum { beneficiary = pkh, deadline = POSIXTime 1760897124, code = 58443 }
vd
```



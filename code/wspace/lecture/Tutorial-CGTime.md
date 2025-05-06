# Detailed Tutorial: Understanding and Using `CGTime.hs`

This tutorial covers the `CGTime.hs` module, detailing its imports, core functions, use cases, and practical examples for handling various date and time operations crucial in blockchain applications, particularly those involving Plutus smart contracts.

## 1. Imports Overview

### Time Manipulation Libraries:

* **Data.Time**:

  * Provides general types and operations for working with date and time (`UTCTime`, `NominalDiffTime`).
* **Data.Time.Clock**:

  * Functions for time arithmetic and obtaining the current system time.
* **Data.Time.Clock.POSIX**:

  * Conversion utilities between POSIX timestamps and `UTCTime`.
* **Data.Time.Format.ISO8601**:

  * Parsing and formatting utilities specifically for ISO-8601 formatted strings.
* **Data.Time.Format**:

  * General parsing and formatting utilities for custom date-time formats.
* **Data.Time.LocalTime**:

  * Functions to handle local time based on the system's timezone.

## 2. Core Functions

### Conversion Functions

* **`utcToPOSIX`**:

  * Converts a UTC timestamp to a POSIX timestamp.
* **`iso8601ToPOSIX`**:

  * Parses ISO-8601 formatted strings to POSIX timestamps.
* **`posixToUTC`**:

  * Converts a POSIX timestamp back to UTC format.
* **`posixToISO8601`**:

  * Formats POSIX timestamps as ISO-8601 strings.

### Current Time Retrieval

* **`getUTCNow`**:

  * Retrieves the current time as `UTCTime`.
* **`getPOSIXNow`**:

  * Retrieves the current time as a POSIX timestamp.
* **`getISO8601Now`**:

  * Retrieves the current time formatted as an ISO-8601 string.
* **`getTimeTriple`**:

  * Provides current time as `UTCTime`, POSIX timestamp, and ISO-8601 string simultaneously.

### Time Arithmetic

* **`addSeconds`, `addSecondsPOSIX`**:

  * Adds a specified number of seconds to `UTCTime` or POSIX timestamps.
* **`addDaysUTC`, `addDaysPOSIX`**:

  * Adds days (converted to seconds) to `UTCTime` or POSIX timestamps.
* **`diffSeconds`, `diffSecondsPOSIX`**:

  * Computes the difference in seconds between two timestamps (`UTCTime` or POSIX).

### Formatting and Parsing

* **`formatUTC`**:

  * Formats `UTCTime` with a custom format string.
* **`parseUTC`**:

  * Parses a string into `UTCTime` using a custom format.
* **`getLocalISO8601`**:

  * Obtains the local time formatted as an ISO-8601 string based on the system's timezone.

## 3. Practical Usage Examples

```haskell
-- Convert current UTC to POSIX timestamp
currentPOSIX :: IO POSIXTime
currentPOSIX = utcToPOSIX <$> getUTCNow

-- Parse ISO-8601 string and convert to POSIX timestamp
parseISO :: Maybe POSIXTime
parseISO = iso8601ToPOSIX "2025-05-04T14:30:00Z"

-- Format current time as ISO-8601
currentISO :: IO String
currentISO = getISO8601Now
```

## 4. Testing Strategy

* Verify round-trip conversions (`UTC` ↔ `POSIX` ↔ `ISO8601`).
* Validate arithmetic operations with various time intervals.
* Test custom formatting and parsing with different time string representations.

## 5. Best Practices

* Always handle possible parsing errors explicitly.
* Regularly validate conversions to ensure they maintain integrity across different system timezones and edge cases.
* Keep your testing thorough and update your tests when modifying related functions to catch regressions early.

## 6. Summary

Here is a detailed tutorial for the `CGTime.hs` module in the style you've requested:

---

# Detailed Tutorial: Understanding and Using `CGTime.hs`

This tutorial breaks down the utility module `CGTime.hs`, explaining its time manipulation functions, how and when to use them, and provides examples for working with UTC, POSIXTime, and ISO8601 time representations in Haskell.

---

## 1. Purpose of the Module

The `CGTime` module offers helper functions for:

* Converting between `UTCTime` and `POSIXTime`
* Working with ISO-8601 formatted strings
* Performing time arithmetic (add/diff days or seconds)
* Getting the current time in multiple formats
* Formatting and parsing time strings

This is especially useful in Cardano/Plutus-based applications where time and timestamps are handled using POSIXTime.

---

## 2. Key Imports Explained

| Module                          | Purpose                                  |
| ------------------------------- | ---------------------------------------- |
| `Data.Time` / `Data.Time.Clock` | Core UTC time management and arithmetic. |
| `Data.Time.Clock.POSIX`         | POSIX time (seconds since Unix epoch).   |
| `Data.Time.Format.ISO8601`      | Parsing/formatting ISO-8601 timestamps.  |
| `Data.Time.Format`              | Custom time format parsing/printing.     |
| `Data.Time.LocalTime`           | Local system time in ISO-8601.           |

---

## 3. Function Categories and Examples

---

### 🔁 **Conversion Between UTC and POSIX**

```haskell
utcToPOSIX :: UTCTime -> POSIXTime
posixToUTC :: POSIXTime -> UTCTime
```

* **Usage**: Convert times to/from Plutus-friendly POSIX format.
* **Example**:

  ```haskell
  posixTime <- utcToPOSIX <$> getCurrentTime
  utcTime   <- pure $ posixToUTC posixTime
  ```

---

### 🕒 **ISO-8601 Integration**

```haskell
iso8601ToPOSIX :: String -> Maybe POSIXTime
posixToISO8601 :: POSIXTime -> String
```

* **Usage**: Convert readable ISO-8601 strings like `"2025-05-04T00:00:00Z"` to `POSIXTime`, and vice versa.
* **Example**:

  ```haskell
  isoTime = "2025-05-04T12:00:00Z"
  Just posix = iso8601ToPOSIX isoTime
  isoBack = posixToISO8601 posix
  ```

---

### 📅 **Current Time Retrieval**

```haskell
getUTCNow :: IO UTCTime
getPOSIXNow :: IO POSIXTime
getISO8601Now :: IO String
getTimeTriple :: IO (UTCTime, POSIXTime, String)
```

* **Usage**: Fetch current time in various formats. `getTimeTriple` gives all three at once.
* **Example**:

  ```haskell
  (utc, posix, iso) <- getTimeTriple
  ```

---

### ➕ ➖ **Time Arithmetic (Add or Subtract)**

```haskell
addSeconds :: NominalDiffTime -> UTCTime -> UTCTime
addSecondsPOSIX :: NominalDiffTime -> POSIXTime -> POSIXTime
addDaysUTC :: Integer -> UTCTime -> UTCTime
addDaysPOSIX :: Integer -> POSIXTime -> POSIXTime
diffSeconds :: UTCTime -> UTCTime -> NominalDiffTime
diffSecondsPOSIX :: POSIXTime -> POSIXTime -> NominalDiffTime
```

* **Usage**: Shift time forward/backward and compute differences.
* **Example**:

  ```haskell
  now <- getPOSIXNow
  let tomorrow = addDaysPOSIX 1 now
  let delta = diffSecondsPOSIX tomorrow now  -- Should be ~86400
  ```

---

### 🧾 **Formatting and Parsing**

```haskell
formatUTC :: String -> UTCTime -> String
parseUTC :: String -> String -> Maybe UTCTime
```

* **Usage**: Format UTC time using custom layouts or parse them from strings.
* **Example**:

  ```haskell
  let s = formatUTC "%F %T" utcTime  -- "2025-05-04 14:30:00"
  parseUTC "%F %T" s
  ```

---

### 🌐 **System Local Time**

```haskell
getLocalISO8601 :: IO String
```

* **Usage**: Get current time formatted using the system’s local timezone.
* **Example**:

  ```haskell
  getLocalISO8601  -- "2025-05-04T15:23:00+03:00"
  ```

---

## 4. Practical Applications

In Plutus contracts:

* Use `POSIXTime` as all on-chain timestamps are in this format.
* For test scripts and CLI tools, convert human-readable `String` to `POSIXTime` with `iso8601ToPOSIX`.
* Use `diffSeconds` or `addDaysPOSIX` to compute vesting periods or lock-up durations.

---

## 5. Summary

This utility module makes working with blockchain timestamps much easier in both on-chain and off-chain code. It wraps verbose `Data.Time` functions in reusable, Plutus-friendly helpers that are useful for:

* Encoding vesting schedules
* Generating transaction validity intervals
* Printing debug timestamps
* Building CLI tools and test scripts

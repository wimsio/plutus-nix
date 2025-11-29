# 🚀 Template Haskell Tutorial: Metaprogramming - Code Generation in Haskell

**Author:** Bernard Sibanda
**Module:** `TemplateHaskellDemo`
**Language Extensions:** `TemplateHaskell`
## 📚 Table of Contents

1. 🔍 Introduction
2. 🧠 What is Template Haskell?
3. ⚙️ How Template Haskell Works (Compile-Time Code Generation)
4. 🧩 Setting up the `TemplateHaskellDemo` Module
5. ✨ Function 1 — `makeHello`: Auto-generate a Greeting Function
6. ➕ Function 2 — `makeAddFunction`: Auto-generate an Addition Function
7. 📦 Function 3 — `makeVolFunction`: Generate a Volume Function
8. 🎓 Function 4 — `makeGradeFunc`: Auto-generate a Grading Function
9. 🔁 Function 5 — `mkTriple`: Generate a Lambda Function Expression
10. 🧪 Using These Functions in Another Module
11. 🧭 Summary Table
12. 🧾 Glossary of Terms
## 1. 🔍 Introduction

Welcome to **Template Haskell**, Haskell’s **metaprogramming system** — the art of writing code that *writes code*.
In this tutorial, you’ll learn how to:

* Generate **functions dynamically** at compile time,
* Use **Template Haskell splices (`$(...)`)**,
* Understand how compile-time macros work safely and purely.
## 2. 🧠 What is Template Haskell?

**Template Haskell (TH)** is Haskell’s **compile-time metaprogramming API**.

It lets you:

* **Generate** code before compilation.
* **Inspect** code (reflection).
* **Transform** code safely (no string-based macros like C).

Template Haskell provides:

* **Quotes** `[| ... |]`, `[d| ... |]`, `[t| ... |]` → for code construction.
* **Splices** `$( ... )` → for inserting generated code.
* **Names and AST types** like `Q Exp`, `Q [Dec]` → for manipulating Haskell syntax trees.
## 3. ⚙️ How Template Haskell Works

When GHC compiles your module, it runs through stages:

```
Parse → Rename → Typecheck → ⏱ Run Template Haskell (Splice Expansion) → Compile → Link
```

That means your TH code executes **during compilation**, not at runtime!
## 4. 🧩 The `TemplateHaskellDemo` Module

Here’s the full source code of your module:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellDemo (
  makeHello, 
  makeAddFunction, 
  makeVolFunction,
  makeGradeFunc,
  mkTriple
) where

import Language.Haskell.TH 

-- 1️⃣ Generate a greeting function
makeHello :: String -> Q [Dec]
makeHello name = [d|
  greet = putStrLn ("Hello " ++ name)
  |]

-- 2️⃣ Generate an addition function
makeAddFunction :: Int -> Int -> Q [Dec]
makeAddFunction a b = [d|
  add = a + b
  |]

-- 3️⃣ Generate a volume function
makeVolFunction :: Q [Dec]
makeVolFunction = [d|
  volFunction :: Int -> Int -> Int -> Int
  volFunction x y z = x * y * z
  |]

-- 4️⃣ Generate a grade evaluation function
makeGradeFunc :: Q [Dec]
makeGradeFunc = [d|
  gradeFunc :: Int -> String
  gradeFunc n
    | n < 25 = "Grade is F"
    | n < 45 = "Grade is E"
    | n < 50 = "Grade is D"
    | n < 60 = "Grade is C"
    | n < 80 = "Grade is B"
    | otherwise = "Grade is A"
  |]

-- 5️⃣ Generate a lambda expression for cube
mkTriple :: Q Exp
mkTriple = [| \x -> x * x * x |]
```
## 5. ✨ Function 1 — `makeHello`

👋 *Auto-generates a function that prints a greeting.*

```haskell
makeHello :: String -> Q [Dec]
makeHello name = [d|
  greet = putStrLn ("Hello " ++ name)
  |]
```

### 🧱 How It Works:

* `[d| ... |]` is a *declaration quote* that produces `Q [Dec]` (a list of top-level declarations).
* Inside it, you define `greet` as if you wrote it manually.
* The argument `name` is inserted at compile time.

### 🧪 Usage:

```haskell
{-# LANGUAGE TemplateHaskell #-}
import TemplateHaskellDemo

$(makeHello "Bernard Sibanda")

main = greet
```

🖨️ **Output:**

```
Hello Bernard Sibanda
```
## 6. ➕ Function 2 — `makeAddFunction`

🧮 *Generates a function that adds two compile-time numbers.*

```haskell
makeAddFunction :: Int -> Int -> Q [Dec]
makeAddFunction a b = [d|
  add = a + b
  |]
```

### 🔍 Explanation:

* You pass two constants (`a`, `b`).
* Template Haskell computes nothing itself — it just injects the code:

  ```haskell
  add = 3 + 5
  ```
* GHC then compiles that normally.

### 🧪 Usage:

```haskell
$(makeAddFunction 23 65)
main = print add
```

🖨️ **Output:**

```
88
```
## 7. 📦 Function 3 — `makeVolFunction`

📏 *Generates a function that calculates volume.*

```haskell
makeVolFunction :: Q [Dec]
makeVolFunction = [d|
  volFunction :: Int -> Int -> Int -> Int
  volFunction x y z = x * y * z
  |]
```

### 💡 Explanation:

* `[d| ... |]` defines a **full function** with a type signature and body.
* It multiplies three parameters.
* No runtime computation — it’s compiled as if you wrote the function yourself.

### 🧪 Usage:

```haskell
$(makeVolFunction)
main = print (volFunction 2 3 4)
```

🖨️ **Output:**

```
24
```
## 8. 🎓 Function 4 — `makeGradeFunc`

📘 *Generates a grading function using compile-time guards.*

```haskell
makeGradeFunc :: Q [Dec]
makeGradeFunc = [d|
  gradeFunc :: Int -> String
  gradeFunc n
    | n < 25 = "Grade is F"
    | n < 45 = "Grade is E"
    | n < 50 = "Grade is D"
    | n < 60 = "Grade is C"
    | n < 80 = "Grade is B"
    | otherwise = "Grade is A"
  |]
```

### 🧠 Explanation:

* This code defines a **function with multiple guarded branches**.
* Template Haskell treats it exactly as if you’d written the definition manually.

### 🧪 Usage:

```haskell
$(makeGradeFunc)
main = putStrLn (gradeFunc 77)
```

🖨️ **Output:**

```
Grade is B
```
## 9. 🔁 Function 5 — `mkTriple`

📈 *Generates a cubic lambda expression.*

```haskell
mkTriple :: Q Exp
mkTriple = [| \x -> x * x * x |]
```

### 🧩 Explanation:

* `[| ... |]` produces a **quoted expression (`Q Exp`)**.
* It defines a **lambda** (`\x -> x * x * x`) that cubes a number.
* You can use it in an expression position.

### 🧪 Usage:

```haskell
triple :: Int -> Int
triple = $(mkTriple)

main = print (triple 3)
```

🖨️ **Output:**

```
27
```
## 10. 🧪 Using All Together

**`Main.hs`:**

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main where

import TemplateHaskellDemo

$(makeHello "Alice")
$(makeAddFunction 10 15)
$(makeVolFunction)
$(makeGradeFunc)

triple :: Int -> Int
triple = $(mkTriple)

main :: IO ()
main = do
  greet
  print add
  print (volFunction 2 3 4)
  putStrLn (gradeFunc 72)
  print (triple 5)
```

🖨️ **Output:**

```
Hello Alice
25
24
Grade is B
125
```
## 11. 🧭 Summary Table

| Function          | Type                    | Generated Code                        | Example Output |              |
| ----------------- | ----------------------- | ------------------------------------- | -------------- | ------------ |
| `makeHello`       | `String -> Q [Dec]`     | `greet = putStrLn ("Hello " ++ name)` | `Hello Alice`  |              |
| `makeAddFunction` | `Int -> Int -> Q [Dec]` | `add = a + b`                         | `88`           |              |
| `makeVolFunction` | `Q [Dec]`               | `volFunction x y z = x * y * z`       | `24`           |              |
| `makeGradeFunc`   | `Q [Dec]`               | `gradeFunc n                          | n<25=...`      | `Grade is B` |
| `mkTriple`        | `Q Exp`                 | `\x -> x * x * x`                     | `125`          |              |
## 12. 🧾 Glossary of Terms

| Term                      | Meaning                                                        |                                              |     |      |                                         |
| ------------------------- | -------------------------------------------------------------- | -------------------------------------------- | --- | ---- | --------------------------------------- |
| **Template Haskell (TH)** | Haskell’s compile-time metaprogramming system.                 |                                              |     |      |                                         |
| **`Q` Monad**             | The compile-time computation context used by Template Haskell. |                                              |     |      |                                         |
| **`Exp`**                 | Abstract syntax tree for expressions.                          |                                              |     |      |                                         |
| **`Dec`**                 | Abstract syntax tree for declarations.                         |                                              |     |      |                                         |
| **Quotation `[            | ...                                                            | ]`/`[d                                       | ... | ]`** | Creates code fragments (as AST values). |
| **Splice `$( ... )`**     | Inserts generated code at compile time.                        |                                              |     |      |                                         |
| **Guard (`                | `)**                                                           | Conditional branch in a function definition. |     |      |                                         |
| **Lambda (`\x -> ...`)**  | Anonymous function syntax.                                     |                                              |     |      |                                         |
| **Metaprogramming**       | Writing code that writes or manipulates other code.            |                                              |     |      |                                         |
## 🎯 Final Thoughts

🔥 Template Haskell lets you:

* Write less boilerplate,
* Create entire functions dynamically,
* Keep code *type-safe and declarative*,
* Run logic *before your program is even compiled*.

It’s one of Haskell’s most powerful — and underused — features.


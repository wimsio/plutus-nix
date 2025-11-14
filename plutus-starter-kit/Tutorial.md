# Coxy Plutus Playground – User Tutorial

## 📑 Table of Contents

1. **📘 What Is Coxy Plutus Playground?**
2. **🚀 Getting Started: First Look at the UI**
3. **🏭 Using Prebuilt Industries (Template Mode)**
4. **🧩 Designing Your Own Contract (Custom Industry Mode)**
5. **📦 Datum Options Panel (State & Parameters)**
6. **🎯 Redeemer Options Panel (Actions & Constraints)**
7. **🛠 Generating JSON Specs & Haskell Code**
8. **📥 Downloading & Using `ValidatorLogic.hs`**
9. **🧪 Example Walkthrough: Simple Escrow Contract**
10. **💡 Tips, Patterns, and Common Gotchas**
11. **📚 Glossary of Terms (Detailed)**

## 1. 📘 What Is Coxy Plutus Playground?

Coxy Plutus Playground is a **UI helper** for designing Plutus smart contracts without starting from a blank Haskell file.

It helps you:

* Choose an **industry / contract template** (DEX, escrow, lending, auctions, etc.), or define your **own custom contract**.
* Select which **datum fields** you want (on-chain state & parameters).
* Define **redeemer actions** (what users can do: `Deposit`, `Withdraw`, `Cancel`, etc.).
* Attach **constraints** to those actions (e.g. *must be signed by buyer*, *before deadline*, *script owns NFT*, etc.).
* Auto-generate:

  * **Datum JSON spec**
  * **Redeemer JSON spec**
  * A **Haskell `mkValidator` preview**
  * A complete **Haskell module** (`ValidatorLogic.hs`) you can download.

Think of it as:

> **A “contract builder” that outputs ready-to-edit Plutus code and specs.**

## 2. 🚀 Getting Started: First Look at the UI

When you open the HTML page, you’ll see a **two-column layout**.

### 🔹 Left Side – Configuration

* **Industry selector** (`<select id="industry">`)
* Optional **“Custom Industry / Custom Contract”** section
  (fields for custom contract title, ID, datum fields, actions, etc.).
* **Datum options panel**
* **Redeemer options panel**
* Optional **advanced custom datum & redeemer** toggles.
* **Generate Logic Spec** button.

### 🔹 Right Side – Outputs

Stacked text areas:

1. **Datum JSON** – describes your chosen datum structure.
2. **Redeemer JSON** – actions + constraints.
3. **Haskell mkValidator (preview)** – the core validator logic skeleton.
4. **Full Haskell Plutus module** – a compilable `ValidatorLogic.hs` template.
   There’s also a **“Download ValidatorLogic.hs”** button.

## 3. 🏭 Using Prebuilt Industries (Template Mode)

This is the default mode when **“Custom Industry” is OFF**.

### 3.1 Choose an Industry

1. At the top left, use **Industry** dropdown.
2. Pick something like **“DEX”, “Escrow”, “Lending”, “Auction”**, etc.
3. When you change the selection:

   * **Datum options** (on the left) update with fields relevant to that contract.
   * **Redeemer options** (below) update with actions and built-in constraints.

### 3.2 Datum Options from Template

In the **Datum options** subpanel:

* You’ll see checkboxes like:

  * `☑ seller :: PubKeyHash`
  * `☐ buyer :: PubKeyHash`
  * `☑ price :: Integer`
  * etc.

* Tick the fields you actually want your contract to use.

* These become the record fields of `CoxyDatum` (or industry-specific type name).

### 3.3 Redeemer Actions & Constraints from Template

In the **Redeemer options** panel:

* Each **action** (e.g. `PaySeller`, `Cancel`, `Bid`, `Close`) appears as a block:

  * Title (action label).
  * A list of **constraints** as checkboxes:

    * `☑ signedBySeller`
    * `☐ beforeDeadline`
    * `☑ scriptHasNFT`
    * etc.

* When you tick constraints for an action:

  * They appear in the **Redeemer JSON** under that action.
  * Each becomes a Haskell call like:

    ```haskell
    traceIfFalse "signedBySeller" (constraint_signedBySeller dat ctx)
    ```

## 4. 🧩 Designing Your Own Contract (Custom Industry Mode)

This is the fun part: **you define the whole contract “shape.”**

### 4.1 Enable Custom Industry Mode

On the left, you’ll have a toggle / checkbox like:

> **Enable custom industry / contract**

When you **check it**:

* The **Industry dropdown is disabled** (grayed out).
* A new **Custom Industry** section appears.
* The prebuilt template fields disappear (so you don’t mix modes by accident).

### 4.2 Custom Contract Metadata

In the **Custom Industry / Contract** section, you’ll see inputs such as:

* **Contract Title**

  * e.g. `Escrow with Milestones`
  * Used for display / docs (and you can later wire it into your Haskell module name if you want).

* **Custom Industry ID**

  * e.g. `escrowMilestone`
  * Used as an internal ID; keep it **no spaces, camelCase or snake_case**.

* **Datum Type Name**

  * e.g. `EscrowDatum`
  * This will be used instead of `CoxyDatum` in Haskell.

* **Redeemer Type Name**

  * e.g. `EscrowRedeemer`
  * This becomes the Haskell `data EscrowRedeemer = ...` type.

### 4.3 Add Custom Datum Fields

In **Custom Datum Fields** section (e.g. `custom-industry-datum-list`):

* Click **“Add datum field”** (or equivalent button).
* For each row, fill in:

  * **Field name** (`seller`, `buyer`, `deadline`, `price`, etc.)
  * **Plutus type** (chosen from a dropdown):

    * `Integer`
    * `PubKeyHash`
    * `POSIXTime`
    * `CurrencySymbol`
    * `TokenName`
    * `BuiltinByteString`
    * `Bool`
    * etc.

Example:

* Field name: `seller`
* Type: `PubKeyHash`

This will generate in Haskell:

```haskell
data EscrowDatum = EscrowDatum
    { cdSeller :: PubKeyHash
    , ...
    }
```

> **Note:** prefix `cd` is added by the generator for field names.

### 4.4 Add Custom Redeemer Actions + Constraints

In **Custom Redeemer Actions** section (e.g. `custom-industry-actions-list`):

* Click **“Add action”** to create a new row.

Each row has:

* **Action name** (constructor name):

  * e.g. `PaySeller`, `Cancel`, `ClaimRefund`
  * These become Haskell constructors: `data EscrowRedeemer = PaySeller | Cancel | ClaimRefund`

* **Label / description (optional)**:

  * Human-friendly text for the UI/JSON, e.g. `Pay seller in full if conditions met`.

* **Constraints (comma-separated IDs)**:

  * A text field where you type constraint IDs like:

    * `scriptHasNFT,signedByBuyer,beforeDeadline`
  * Each one becomes:

    * An entry in JSON under the action’s `constraints`.
    * A Haskell trace line inside the matching `case` arm:

      ```haskell
      PaySeller ->
            traceIfFalse "scriptHasNFT"       (constraint_scriptHasNFT dat ctx)
        &&  traceIfFalse "signedByBuyer"     (constraint_signedByBuyer dat ctx)
        &&  traceIfFalse "beforeDeadline"    (constraint_beforeDeadline dat ctx)
      ```

The generator also emits **stub functions** like:

```haskell
constraint_scriptHasNFT :: EscrowDatum -> ScriptContext -> Bool
constraint_scriptHasNFT _ _ = True
```

so you can later implement the actual logic.


## 5. 📦 Datum Options Panel (State & Parameters)

This panel is the **“what does the contract know?”** section.

You’ll see:

* In **template mode**:

  * Checkboxes for existing fields defined by that industry.
* In **custom mode**:

  * The template checkboxes are not used; instead, all fields you added in custom UI are taken automatically.

### 5.1 Optional Advanced Custom Datum Fields

There’s also an **“Enable custom datum fields (advanced)”** toggle:

* When enabled, a section appears where you can add extra fields as free-form text:

  * Field name
  * Haskell type (manually typed)

This is useful when you want fields that:

* Aren’t part of the industry template.
* Are of more complex types you don’t want to expose in the main dropdown (e.g. `Maybe POSIXTime`, or custom sum types).

These **advanced** fields go into `customFields` in the **Datum JSON** and into the Haskell record as extra fields.

## 6. 🎯 Redeemer Options Panel (Actions & Constraints)

This is the **“what can users do?”** section.

### 6.1 Actions

Each **action** (e.g. `Bid`, `Close`, `Liquidate`, `Deposit`, `Withdraw`) becomes:

* A constructor in your redeemer type (e.g. `data AuctionRedeemer = Bid | Close`).
* A `case` branch in the generated `mkValidator`:

  ```haskell
  mkValidator dat red ctx =
    case red of
      Bid   -> ...
      Close -> ...
  ```

### 6.2 Constraints (Template Mode)

For prebuilt industries:

* Each action block has checkboxes for **reusable constraint IDs** like:

  * `signedBySeller`
  * `signedByBuyer`
  * `beforeDeadline`
  * `hasCorrectPrice`
  * etc.

Ticking them:

* Adds them to **Redeemer JSON** under that action.
* Generates `traceIfFalse` checks that **call stub functions** like `constraint_signedBySeller`.

### 6.3 Constraints (Custom Mode)

For custom actions:

* You type constraints in the **Constraints** text field, **comma-separated**:

  * `scriptHasNFT,signedByBuyer,beforeDeadline`
* The generator splits on commas, trims whitespace, and uses each non-empty token as a constraint ID.
* They behave exactly like template constraints (they just don’t come from a pre-defined list).


## 7. 🛠 Generating JSON Specs & Haskell Code

Once you’ve configured your datum and actions:

1. Click **“Generate Logic Spec”**.

The script:

1. Chooses **industry**:

   * Template mode → from selected dropdown.
   * Custom mode → from `getCustomIndustryFromUI()`.

2. Builds:

   * `datumSpec = buildDatumSpec(industry)`
   * `redeemerSpec = buildRedeemerSpec(industry)`

3. Writes to outputs:

   * **Datum JSON** (`datum-json-output`):

     ```json
     {
       "datumType": "EscrowDatum",
       "fields": [
         { "id": "seller", "type": "PubKeyHash", "label": "seller :: PubKeyHash" },
         ...
       ],
       "customFields": []
     }
     ```

   * **Redeemer JSON** (`redeemer-json-output`):

     ```json
     {
       "redeemerType": "EscrowRedeemer",
       "actions": [
         {
           "id": "PaySeller",
           "label": "Pay seller in full",
           "constraints": [
             "signedByBuyer",
             "beforeDeadline"
           ]
         }
       ],
       "customActions": []
     }
     ```

4. Generates Haskell:

   * **Preview** `mkValidator` (3rd box)
   * **Full module** (4th box) via `mkFullHaskellModule` – including:

     * `data <DatumType>`
     * `data <RedeemerType>`
     * `mkValidator`
     * Stubs `constraint_<id>` for each constraint.


## 8. 📥 Downloading & Using `ValidatorLogic.hs`

When you are happy with the generated module:

1. Click **“Download ValidatorLogic.hs”**.
2. The browser will save a file named `ValidatorLogic.hs`.
3. Move this into your **Plutus project** (e.g. under `src/`).
4. Wire it into your build:

   * Import it from your main validator builder.
   * Use `mkValidator` with `PlutusTx.compile`.
   * Wrap using `mkValidatorScript` / `TypedValidator` patterns depending on your framework.

Then you can:

* Use the generated script on a **testnet** or **local emulator**.
* Modify the stubs for constraints to implement real logic.


## 9. 🧪 Example Walkthrough: Simple Escrow Contract

Let’s do a short, concrete run:

### 9.1 Choose Mode

* Turn **ON** “Custom Industry / Contract”.

### 9.2 Metadata

* Contract Title: `Simple Escrow`
* Industry ID: `simpleEscrow`
* Datum Type: `EscrowDatum`
* Redeemer Type: `EscrowRedeemer`

### 9.3 Datum Fields

Add 4 fields:

1. `seller :: PubKeyHash`
2. `buyer  :: PubKeyHash`
3. `price  :: Integer`
4. `deadline :: POSIXTime`

### 9.4 Redeemer Actions

Add 2 actions:

1. **Name:** `PaySeller`
   **Label:** `Release funds to seller`
   **Constraints:** `signedByBuyer,beforeDeadline`

2. **Name:** `RefundBuyer`
   **Label:** `Refund buyer after deadline`
   **Constraints:** `signedBySeller,afterDeadline`

### 9.5 Generate

Click **Generate Logic Spec**.

Check:

* Datum JSON: has `seller`, `buyer`, `price`, `deadline`.
* Redeemer JSON:

  * `PaySeller` has constraints `signedByBuyer`, `beforeDeadline`.
  * `RefundBuyer` has `signedBySeller`, `afterDeadline`.

Open **Haskell preview**:

* See `EscrowDatum` and `EscrowRedeemer` types.
* `mkValidator` with two branches:

  ```haskell
  mkValidator dat red ctx =
    case red of
      PaySeller ->
            traceIfFalse "signedByBuyer" (constraint_signedByBuyer dat ctx)
        &&  traceIfFalse "beforeDeadline" (constraint_beforeDeadline dat ctx)
      RefundBuyer ->
            traceIfFalse "signedBySeller" (constraint_signedBySeller dat ctx)
        &&  traceIfFalse "afterDeadline" (constraint_afterDeadline dat ctx)
  ```

In the full module, fill in constraint stubs with real logic, e.g.:

```haskell
constraint_signedByBuyer :: EscrowDatum -> ScriptContext -> Bool
constraint_signedByBuyer dat ctx =
  txSignedBy (scriptContextTxInfo ctx) (cdBuyer dat)

constraint_beforeDeadline :: EscrowDatum -> ScriptContext -> Bool
constraint_beforeDeadline dat ctx =
  contains (to (cdDeadline dat)) (txInfoValidRange (scriptContextTxInfo ctx))
```

…and so on.


## 10. 💡 Tips, Patterns, and Common Gotchas

### ✅ Good Practices

* **Use PascalCase for type names:** `EscrowDatum`, `EscrowRedeemer`.
* **Use ConstructorCase** for actions: `Open`, `Close`, `Cancel`, `Bid`.
* Keep **constraint IDs short but meaningful**:

  * `signedBySeller`, `beforeDeadline`, `hasScriptNFT`, `paysExactPrice`.

### ⚠️ Common Gotchas

* **No fields in custom datum** → generator will alert you.
* **No actions in custom mode** → generator will alert you.
* **Typos in Plutus types** (in advanced custom datum fields) won’t be checked until GHC/Plutus compile time.
* **Constraints do nothing at first** – they’re **stubs** returning `True` until you implement them.


## 11. 📚 Glossary of Terms (Detailed)

### 🔹 Datum

The **on-chain state / parameters** attached to a UTxO at a script address.

* In Plutus V2, it is a typed Haskell value (e.g. `EscrowDatum`) that is serialized and stored on-chain.
* In this playground, the datum is described by:

  * Datum type name (`EscrowDatum`)
  * Fields (`seller :: PubKeyHash`, `price :: Integer`, etc.).

### 🔹 Redeemer

The **“instruction”** passed when a script UTxO is spent.

* It tells the validator **which action** is being performed.
* In this tool, each redeemer constructor corresponds to an action (`PaySeller`, `RefundBuyer`, `Bid`, etc.).

### 🔹 Industry

A **template profile** for a family of contracts:

* e.g. DEX, escrow, lending, auction, RFQ, oracle, etc.
* Each has:

  * Common datum fields
  * Standard actions
  * A library of constraints

### 🔹 Custom Industry / Custom Contract

A fully **user-defined profile**:

* You choose:

  * Type names
  * Datum fields + types
  * Redeemer actions
  * Constraint IDs per action
* The playground generates all the scaffolding for you.

### 🔹 Constraint

A reusable **predicate stub** representing a rule you want for an action.

Examples:

* `signedByBuyer` → the transaction must be signed by `cdBuyer dat`.
* `beforeDeadline` → the validity range must be before `cdDeadline dat`.
* `scriptHasNFT` → the script must control a specific NFT.

Each constraint becomes:

* An ID string in JSON.
* A `traceIfFalse` line in `mkValidator`.
* A stub Haskell function `constraint_<id>` to implement.

### 🔹 `mkValidator`

The core **validator function**:

```haskell
mkValidator :: DatumType -> RedeemerType -> ScriptContext -> Bool
```

* Determines whether spending the script UTxO is **allowed**.
* Returns `True` to accept, `False` to reject (via on-chain `error`).

### 🔹 `ScriptContext`

Information about the **spending transaction**:

* Inputs / outputs
* Signatories
* Validity interval
* Values being moved
* Etc.

Constraints use it heavily (e.g. verifying signatures, deadlines, amounts).

### 🔹 `traceIfFalse`

A Plutus helper:

```haskell
traceIfFalse :: BuiltinString -> Bool -> Bool
```

* If condition is `False`, logs the message and returns `False`.
* Used for debugging and readable error messages.

### 🔹 `CoxyDatum`, `CoxyRedeemer`

Default type names when an industry / custom contract does not override type names.

* In your custom contract you usually replace them with:

  * `EscrowDatum`, `EscrowRedeemer`
  * `DexDatum`, `DexRedeemer`
  * etc.


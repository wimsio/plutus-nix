**Title : Coxy Haskell Plutus Builder (CHPB)**
**Company : Coxygen Global (Pty) Ltd**
**Author : Bernard Sibanda**
**Date : 20 November 2025**

## Table of Contents

1. 📘 Introduction to Coxy Haskell Plutus Builder
2. 🧭 Core Concepts: Industries, Datum, Redeemer and Constraints
3. 🖥️ Getting Started with the CHPB Interface
4. 🔍 Searching and Selecting a Smart Contract Industry
5. 🧱 Working with Template Industries
6. 🧮 Configuring Coxy Datum and Custom Datum Fields
7. 🎛️ Configuring Coxy Redeemer and Action Constraints
8. 🧩 Using Parameter Datum (CoxyParamDatum)
9. 🧪 Creating a Custom Smart Contract Industry
10. ⚙️ Generating JSON Specifications and Haskell Code
11. 📂 Downloading and Integrating `ValidatorLogic.hs`
12. 🧷 End-to-End Example: Coxy / Marketplace Escrow
13. 🛠️ Troubleshooting and Practical Tips
14. 📚 Glossary of Terms

### 1. 📘 Introduction to Coxy Haskell Plutus Builder

Coxy Haskell Plutus Builder (CHPB) is an interactive web-based tool that helps you design Plutus validator logic without manually wiring every data type and constraint from scratch. It gives you a curated library of “industries” such as marketplaces, auctions, AMMs and credit primitives, each with pre-defined datum fields, redeemer actions and safety constraints. From a single screen, you can select the industry you want, choose the data you care about, enable or disable constraints, and then generate machine-readable JSON specifications together with a ready-to-integrate Haskell module containing your `mkValidator` skeleton and associated types. 

CHPB has two modes of use. The first mode is template-based, where you simply pick one of the pre-packaged industries such as “Coxy / Marketplace”, “Membership SBT / DAO” or “Stable-swap AMM (Curve-like)” and specialise it by ticking fields and constraints.  The second mode is fully custom, where you define your own contract title, datum type, redeemer type, datum fields and actions from scratch in a guided way, while still benefiting from the same export pipeline to JSON and Haskell code. 


### 2. 🧭 Core Concepts: Industries, Datum, Redeemer and Constraints

In CHPB, an “industry” represents a reusable contract archetype for a particular DeFi or Web3 use case. Each industry entry in `coxy-industries.js` specifies a datum type name, a redeemer type name, a list of datum fields, a list of redeemer actions and a set of constraints that can be attached to each action. For example, the “Coxy / Marketplace” industry defines a `CoxyDatum` with fields such as `seller`, `buyer`, `amount` and `deadline`, and a `CoxyRedeemer` with actions like `PaySeller` and `RefundBuyer`. 

A datum field is a typed on-chain value that describes the state of a contract instance, such as the seller’s public key hash or an auction deadline. Redeemer actions represent user-triggered events or flows, such as joining a DAO, placing a bid or executing a time-weighted order. Constraints are named logical conditions which will be checked in the final validator, for example “signedByBuyer” or “currentPriceComputedCorrectly”.  In CHPB you bind constraints to actions using simple checkboxes, and the builder later converts this selection into Haskell `traceIfFalse` checks that call corresponding constraint functions like `constraint_signedByBuyer dat ctx`. 


### 3. 🖥️ Getting Started with the CHPB Interface

The CHPB interface is a single-page layout with a header, three-step top row, configuration panels for datum and redeemer, a preview area and an output section for JSON and Haskell code.  The layout is styled with a modern card-based design and rounded buttons so that each logical block of the workflow is visually separated.  At the very top you will see the title “Coxy Haskell Plutus Builder” which introduces the page as the main playground for your contract design session.

Below the header, the interface walks you through a natural workflow: first you search or select an industry, then you adjust datum and redeemer settings, optionally configure parameter datum fields, and finally you click “Generate Logic Spec” to populate the JSON and Haskell output areas.  A dedicated “Download ValidatorLogic.hs” button allows you to export the generated module as a file which you can drop directly into your Haskell project.


### 4. 🔍 Searching and Selecting a Smart Contract Industry

The first row of CHPB contains three blocks that guide how you select or define your contract. The left block contains a search input labelled “1. Search Smart Contract Or Industry”. This search box filters the available industries by both their identifier and their label as you type, so you can quickly narrow down to, for example, “englishAuction”, “stableSwapAmm” or “membership” by entering any part of the industry name.

The middle block is titled “2. Smart Contract Industry” and presents a `<select>` dropdown that lists all industries currently defined in `coxy-industries.js`.  When the page loads, CHPB populates this dropdown and automatically selects the first industry in the list, rendering its datum fields and actions below.  Whenever you change the selection, the builder re-renders the datum and action panels and clears previously generated outputs so that your JSON and Haskell code always match the currently selected industry.

The right block is titled “3. Create Custom Smart Contract” and introduces the custom mode toggle.  When you enable this square toggle, CHPB hides the standard template panels, disables the industry dropdown and search box, and reveals a dedicated area for defining your own industry, including custom datum and redeemer types, fields and actions.


### 5. 🧱 Working with Template Industries

In template mode, CHPB uses the industries defined in `coxy-industries.js` as blueprints. Each template describes the structure of the datum, the available actions and the default constraints for those actions. For example, the “Membership SBT / DAO” industry defines a `MemberDatum` with fields such as `org`, `member`, `joined`, `expiry` and `level`, and actions like `Join`, `Renew` and `Revoke`.  Similarly, the “Stable-swap AMM (Curve-like)” industry defines a `StablePoolDatum` with fields like `ampParam`, `feeBps` and `adminRef` and actions including `Swap`, `AddLiq` and `RemoveOne`. 

When you select a template industry, CHPB renders a labelled checkbox for every datum field in that industry in the “Coxy Datum” panel.  You decide which fields you want to include in your specialised contract by ticking their checkboxes. For the actions, CHPB renders blocks in the “Coxy Redeemer” panel, one block per action, each containing constraint checkboxes that are pre-checked according to the `defaultActionConstraints` configuration in `coxy-industries.js`.  In this way, the template industries give you a rich starting point while leaving you free to simplify or tighten the logic as needed.


### 6. 🧮 Configuring Coxy Datum and Custom Datum Fields

The “Coxy Datum” panel is where you configure the on-chain datum type for your contract. Initially, the panel shows the template datum fields as a list of labelled checkboxes such as “seller :: PubKeyHash” or “deadline :: POSIXTime”.  When you generate the logic spec, CHPB will take only the checked fields and include them in the `fields` array of the datum specification, preserving the type information for each field. 

Below the template fields, there is a toggle labelled “Enable Custom Datum Fields (advanced users)”.  When you enable this toggle, a sub-section appears that allows you to add arbitrary extra datum fields using text inputs and a type dropdown, which is pre-populated with a canonical list of Plutus types like `Integer`, `Bool`, `PubKeyHash`, `POSIXTime`, `Value`, `CurrencySymbol` and so on.  Each row you add contributes a `customFields` entry to the datum specification containing a `name` and a `type`.  This mechanism is useful when your desired contract is mostly covered by a template but requires one or two project-specific fields that are not part of the base industry.


### 7. 🎛️ Configuring Coxy Redeemer and Action Constraints

The “Coxy Redeemer” panel is where you shape the behaviour of the contract by configuring which constraints apply to each redeemer action. For every action defined by the selected industry, CHPB renders a small card that shows the action’s label at the top and then lists all available constraints with checkboxes.  Each checkbox expresses whether the constraint’s predicate should be enforced when that action is used at validation time. For example, the “Coxy / Marketplace” action `PaySeller` may include constraints like “scriptHasNFT”, “signedByBuyer” and “sellerPaid”, which ensure the payment is authorised and matches the expected amounts. 

When you click “Generate Logic Spec”, CHPB reads the checked constraints for each action and constructs a redeemer specification. For template industries, each action entry in the redeemer spec has an `id`, a human-readable `label` and a `constraints` array containing the identifiers of the selected constraints.  This redeemer spec is later used to generate the Haskell `mkValidator` case branches, where each action pattern is followed by a chain of `traceIfFalse` calls for the enabled constraints.

A second toggle labelled “Enable Custom Redeemer Actions (advanced-users)” allows you to define additional actions that are not part of the industry template.  When you enable this toggle, you can add rows containing an action name and an optional description. These custom actions are captured in a separate `customActions` array on the redeemer spec, and in the Haskell preview each custom action becomes a case branch that simply uses a generic `traceIfFalse` guard, ready for you to customise in your final Plutus code.


### 8. 🧩 Using Parameter Datum (CoxyParamDatum)

Beyond the primary datum type, CHPB supports an optional parameter datum type called `CoxyParamDatum`. The purpose of this parameter datum is to model contract-level parameters that are fixed at script instantiation time but are not meant to vary between UTxOs, such as a protocol fee, a fixed beneficiary or a governance address. In the “Coxy Datum” panel you will find a separate toggle labelled “Generate parameter datum type (CoxyParamDatum)”.  When enabled, the validator type changes from `CoxyDatum -> CoxyRedeemer -> ScriptContext -> Bool` to `CoxyParamDatum -> CoxyDatum -> CoxyRedeemer -> ScriptContext -> Bool`, and the user interface reveals a dedicated section where you can define parameter fields.

Each parameter row consists of a field name and a Haskell type, both captured through text inputs with type options built from the same Plutus type list used in other parts of the tool.  When you generate the specification, CHPB builds a `CoxyParamDatum` spec containing an array of fields. If you enable parameter datum but provide no fields, the tool warns you and returns `null` to avoid generating an inconsistent type.  In the Haskell preview, the parameter datum name appears both in the type signature and as an extra argument in the `mkValidator` definition, giving you a clear scaffold to plug your own parameter logic into. 


### 9. 🧪 Creating a Custom Smart Contract Industry

When you need a contract that is not well represented by any of the existing industries, you can switch into custom mode using the “Create Custom Smart Contract” toggle in the top row.  Once enabled, the standard datum and redeemer panels are hidden, the industry search and dropdown are disabled, and a new “Custom Contract Title” section appears. Here you specify the contract’s title for the user interface and the Haskell module name, as well as explicit names for the datum and redeemer types.

To define your custom datum, you use the “Custom Datum” column in the custom row. You add as many fields as you need, each with a field name and a Plutus type selected from the shared type dropdown.  These entries are converted directly into `datumFields` with a label string of the form `name :: type`, so that your generated module can show meaningful comments and field descriptions. For actions, the “Custom Redeemer” column allows you to define each action’s identifier and an optional human-readable label.  When you click “Generate Logic Spec”, CHPB validates that at least one datum field and one action have been defined; otherwise, it displays a clear alert and cancels the generation to prevent malformed specifications. 

Internally, CHPB assembles a synthetic industry object from the custom inputs, marking it with `isCustom: true` so that downstream functions such as `buildDatumSpec` and `buildRedeemerSpec` know to use the custom fields and actions directly rather than relying on template checkboxes or constraint lists.  This means that the rest of the pipeline—from building JSON to generating Haskell—works identically whether you are using a template industry or a custom one.


### 10. ⚙️ Generating JSON Specifications and Haskell Code

Once you are satisfied with your datum, redeemer and optional parameter configuration, you click the “Generate Logic Spec” button located beneath the preview panel.  When this button is pressed, CHPB assembles the current industry (template or custom), builds a datum specification, a parameter datum specification if requested, and a redeemer specification, and then serialises them to JSON and Haskell code.

For the datum JSON, the tool constructs a single object that includes the datum type name, the list of fields defined from template and custom entries, and an embedded `paramDatum` section when parameter datum is enabled.  Likewise, the redeemer JSON includes the redeemer type name, a list of actions each with their chosen constraints, and an optional `customActions` list that records any advanced additional actions you defined via the custom redeemer interface.

Simultaneously, CHPB creates a concise “Haskell mkValidator Preview” and a full “Haskell Plutus Module”. The preview focuses on the type signature and the pattern matching over redeemer actions, making it easy to see at a glance how your selected constraints will be turned into `traceIfFalse` checks.  The full module extends this preview by including data type declarations for the datum, parameter datum and redeemer; utility imports; and placeholder constraint function stubs corresponding to each constraint identifier in your industry. 


### 11. 📂 Downloading and Integrating `ValidatorLogic.hs`

The last row of the CHPB interface contains a large “Full Haskell Plutus Module” textarea and a “Download ValidatorLogic.hs” button.  After generating the logic spec, the full module text area is populated with the complete Haskell source for your contract’s validator logic. At this point, you have two options. You may copy and paste the contents into your existing Haskell project manually, or you can click the download button to save the file as `ValidatorLogic.hs` directly.

When you click the download button, CHPB verifies that the full output is non-empty and, if necessary, reminds you to click “Generate Logic Spec” first. It then creates a Blob object with the Haskell content, attaches it to a temporary `<a>` element, triggers a simulated click to download the file, and finally cleans up the temporary URL.  In your local project, this module can now be compiled with your Plutus or Cardano infrastructure as you normally would, allowing you to move seamlessly from UI-driven design to on-chain deployment.


### 12. 🧷 End-to-End Example: Coxy / Marketplace Escrow

To illustrate the workflow, consider building a simple marketplace escrow where the buyer locks funds and the seller receives payment once certain conditions are met. You start by typing “Coxy” into the search box or selecting “Coxy / Marketplace” from the industry dropdown.  The datum panel now shows fields such as `seller :: PubKeyHash`, `buyer :: PubKeyHash`, `amount :: Integer`, `deadline :: POSIXTime`, `currency :: CurrencySymbol` and `token :: TokenName`.

For a straightforward escrow, you might select `seller`, `buyer`, `amount` and `deadline`, while perhaps omitting token-level fields if you are only handling ADA. In the redeemer panel, you will see actions `PaySeller` and `RefundBuyer` each with associated constraints.  For `PaySeller`, you keep constraints like “scriptHasNFT”, “signedByBuyer” and “sellerPaid” checked so that only the correct buyer can pay and the script UTxO must own the expected NFT. For `RefundBuyer`, you keep “scriptHasNFT”, “signedBySeller”, “buyerRefunded” and “afterDeadline” to ensure that refunds occur only with the seller’s participation and after the escrow deadline. 

If your protocol also requires an additional audit key, you can enable custom datum fields and add a `auditor :: PubKeyHash` field via the custom datum section.  Once you are satisfied with the selections, you click “Generate Logic Spec”. The datum JSON now describes a `CoxyDatum` containing your chosen fields and the extra auditor field, the redeemer JSON describes the constraints bound to `PaySeller` and `RefundBuyer`, and the Haskell preview shows `mkValidator dat red ctx` branching on each action with appropriate `traceIfFalse` constraint checks.  You can then review the full module, download `ValidatorLogic.hs` and integrate it into your real Plutus project, adjusting the bodies of the constraint functions to match your precise validation logic and ledger operations.


### 13. 🛠️ Troubleshooting and Practical Tips

If you switch between industries or between template and custom modes and notice that your outputs no longer match your configuration, remember that CHPB purposely clears the JSON and Haskell outputs every time the selected industry or mode changes.  This is by design and ensures that you never accidentally deploy code that was generated for a different configuration. Simply review your selections and click “Generate Logic Spec” again to refresh the outputs.

If search returns no industries for the term you entered, the dropdown will show a disabled “No results” option and the datum and action panels will be cleared.  In that case, either adjust your search term or remove it entirely to restore the full list. When defining parameter datum fields or custom industries, always check that you have at least one field and, for custom industries, at least one action defined. Otherwise, CHPB will show an alert and abort the generation to protect you from producing unusable specifications.

Finally, if clicking “Download ValidatorLogic.hs” does nothing, it usually means that you have not yet generated the logic spec in the current session, or that the full module output has been cleared by mode changes. Click “Generate Logic Spec” once more to fill the Haskell module area and then repeat the download step.


### 14. 📚 Glossary of Terms

**Industry**
An industry is a reusable smart contract archetype defined in `coxy-industries.js`. It encapsulates a use case such as a marketplace, auction, AMM, DAO or credit primitive, and specifies the datum type, redeemer type, list of datum fields, actions and default constraints for that use case.

**Datum**
A datum is the on-chain data record associated with a particular contract instance. CHPB models the datum as a Haskell data type with named fields, such as `seller :: PubKeyHash` or `deadline :: POSIXTime`, and exposes these fields in the UI as checkboxes and custom entries. Only the fields you select or define become part of the generated datum specification and Haskell type.

**Redeemer**
A redeemer represents the action or intent of a transaction that interacts with the contract. In CHPB, redeemers are grouped into a type like `CoxyRedeemer` or `MemberAction` and are rendered as individual action blocks in the “Coxy Redeemer” panel. Each action, such as `PaySeller` or `Join`, may carry its own set of constraints that describe what must hold true when that action is used.

**Constraint**
A constraint is a named logical requirement that should be enforced by the validator for a given action. Examples include `signedByBuyer`, `currentPriceComputedCorrectly` or `stableSwapWithinTolerance`. Constraints are configured per action via checkboxes in the UI and then turned into Haskell `traceIfFalse` guards that call corresponding constraint functions.

**CoxyParamDatum (Parameter Datum)**
CoxyParamDatum is an optional parameter-level datum type which models configuration parameters that are fixed for a script instance, such as fee schedules or governance addresses. When enabled, it appears as an extra argument in the validator type and is defined through its own list of fields in the parameter datum section. The tool enforces that at least one field be defined when parameter datum is enabled, otherwise it returns `null` and warns the user.

**Custom Industry**
A custom industry is a contract definition you build yourself in CHPB by specifying the contract title, industry identifier, label, datum type, redeemer type, datum fields and actions. It is created when the “Create Custom Smart Contract” toggle is enabled and is tagged internally with `isCustom: true` so that the generation pipeline treats its fields and actions differently from template industries while still producing fully compatible JSON and Haskell outputs.

**Generate Logic Spec**
“Generate Logic Spec” is the primary action button that compiles your selections into a coherent specification. When clicked, it reads the current industry (template or custom), builds datum, parameter datum and redeemer specs and fills the Datum JSON, Redeemer JSON, Haskell mkValidator preview and Full Haskell Plutus Module outputs. It also ensures that any mode or industry changes are reflected consistently in the generated artefacts.

**ValidatorLogic.hs**
`ValidatorLogic.hs` is the name of the downloadable Haskell file generated by CHPB. It contains the validator type signature, data type declarations, pattern matching skeleton over redeemer actions and placeholder calls to named constraint functions. This file is intended to be integrated into your Plutus or Cardano project where you will implement the actual constraint logic and compile the module into an on-chain script.

With this tutorial and glossary, a new user should be able to use Coxy Haskell Plutus Builder end to end: from selecting or defining an industry, through configuring datum and redeemer logic, to generating and integrating a complete Haskell validator module into their Cardano development workflow.

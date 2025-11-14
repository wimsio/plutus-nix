// Coxypp.js (ES module)
import { industries } from "./coxy-industries.js";

window.addEventListener("DOMContentLoaded", function () {
  // -------------------------------------------------------------------
  // DOM refs
  // -------------------------------------------------------------------
  const industrySelect     = document.getElementById("industry");
  const datumArea          = document.getElementById("datum-area");
  const actionsArea        = document.getElementById("actions-area");
  const generateBtn        = document.getElementById("generate");

  const datumJsonOutput    = document.getElementById("datum-json-output");
  const redeemerJsonOutput = document.getElementById("redeemer-json-output");
  const haskellOutput      = document.getElementById("haskell-output");
  const haskellFullOutput  = document.getElementById("haskell-full-output");
  const downloadBtn        = document.getElementById("download-hs");

  // Advanced "extra datum fields"
  const customDatumToggle  = document.getElementById("enable-custom-datum");
  const customDatumSection = document.getElementById("custom-datum-section");
  const customDatumList    = document.getElementById("custom-datum-list");
  const addCustomDatumBtn  = document.getElementById("add-custom-datum");

  // Advanced "extra redeemer actions"
  const customRedeemerToggle   = document.getElementById("enable-custom-redeemer");
  const customRedeemerSection  = document.getElementById("custom-redeemer-section");
  const customRedeemerList     = document.getElementById("custom-redeemer-list");
  const addCustomRedeemerBtn   = document.getElementById("add-custom-redeemer");

  // -------------------------------------------------------------------
  // Custom industry UI
  // -------------------------------------------------------------------
  const enableCustomIndustry          = document.getElementById("enable-custom-industry");
  const customIndustrySection         = document.getElementById("custom-industry-section");
  const customIndustryDatumList       = document.getElementById("custom-industry-datum-list");
  const addCustomIndustryDatumBtn     = document.getElementById("add-custom-industry-datum");
  const customIndustryActionsList     = document.getElementById("custom-industry-actions-list");
  const addCustomIndustryActionBtn    = document.getElementById("add-custom-industry-action");

  // Row factories for custom industry
  function createCustomIndustryDatumRow() {
    const row = document.createElement("div");
    row.className = "custom-prop-row custom-industry-datum-row";
    row.innerHTML = `
      <input
        type="text"
        class="cid-name"
        placeholder="Field name e.g. seller"
      />
      <select class="cid-type">
        <option value="">Select Plutus type…</option>
        <option>Integer</option>
        <option>PubKeyHash</option>
        <option>POSIXTime</option>
        <option>CurrencySymbol</option>
        <option>TokenName</option>
        <option>BuiltinByteString</option>
        <option>Bool</option>
      </select>
    `;
    return row;
  }

  function createCustomIndustryActionRow() {
    const row = document.createElement("div");
    row.className = "custom-prop-row custom-industry-action-row";
    row.innerHTML = `
      <input
        type="text"
        class="cia-name"
        placeholder="Action name e.g. PaySeller"
      />
      <input
        type="text"
        class="cia-label"
        placeholder="Label / description (optional)"
      />
      <input
        type="text"
        class="cia-constraints"
        placeholder="Constraints (comma-separated IDs, e.g. signedByBuyer,beforeDeadline)"
      />
    `;
    return row;
  }

  // Toggle visibility and disable built-in industry select
  if (enableCustomIndustry && customIndustrySection) {
    enableCustomIndustry.addEventListener("change", () => {
      const enabled = enableCustomIndustry.checked;
      customIndustrySection.classList.toggle("hidden", !enabled);
      industrySelect.disabled = enabled;

      // Clear left & right panels when switching modes
      datumArea.innerHTML   = "";
      actionsArea.innerHTML = "";
      datumJsonOutput.value    = "";
      redeemerJsonOutput.value = "";
      if (haskellOutput)     haskellOutput.value = "";
      if (haskellFullOutput) haskellFullOutput.value = "";

      // If switching back to normal mode, re-render selected industry
      if (!enabled && industrySelect.value) {
        renderIndustry(industrySelect.value);
      }
    });
  }

  // Add new datum rows in custom industry section
  if (addCustomIndustryDatumBtn && customIndustryDatumList) {
    addCustomIndustryDatumBtn.addEventListener("click", () => {
      customIndustryDatumList.appendChild(createCustomIndustryDatumRow());
    });
  }

  // Add new actions in custom industry section
  if (addCustomIndustryActionBtn && customIndustryActionsList) {
    addCustomIndustryActionBtn.addEventListener("click", () => {
      customIndustryActionsList.appendChild(createCustomIndustryActionRow());
    });
  }

  // Build an "industry" object from the custom industry UI
  function getCustomIndustryFromUI() {
    const titleInput       = document.getElementById("custom-contract-title");
    const customIdInput    = document.getElementById("custom-industry-id");
    const customLabelInput = document.getElementById("custom-industry-label");
    const datumTypeInput   = document.getElementById("custom-datum-type");
    const redeemerTypeInput= document.getElementById("custom-redeemer-type");

    const title        = (titleInput?.value || "").trim() || "CustomContract";
    const id           = (customIdInput?.value || "").trim() || "customContract";
    const label        = (customLabelInput?.value || "").trim() || title;
    const datumType    = (datumTypeInput?.value || "").trim() || "CustomDatum";
    const redeemerType = (redeemerTypeInput?.value || "").trim() || "CustomRedeemer";

    // Datum fields from custom industry section
    const datumRows = Array.from(
      customIndustryDatumList?.querySelectorAll(".custom-industry-datum-row") || []
    );

    const datumFields = datumRows
      .map(row => {
        const nameEl = row.querySelector(".cid-name");
        const typeEl = row.querySelector(".cid-type");
        const name = (nameEl?.value || "").trim();
        const type = (typeEl?.value || "").trim();
        if (!name || !type) return null;
        return {
          id: name,
          label: `${name} :: ${type}`,
          type
        };
      })
      .filter(Boolean);

    // Redeemer actions from custom industry section
    const actionRows = Array.from(
      customIndustryActionsList?.querySelectorAll(".custom-industry-action-row") || []
    );

    const actions = actionRows
      .map(row => {
        const nameEl        = row.querySelector(".cia-name");
        const labelEl       = row.querySelector(".cia-label");
        const constraintsEl = row.querySelector(".cia-constraints");

        const name      = (nameEl?.value || "").trim();
        const labelText = (labelEl?.value || "").trim();
        const rawConstraints = (constraintsEl?.value || "").trim();

        if (!name) return null;

        const constraints =
          rawConstraints
            ? rawConstraints
                .split(",")
                .map(s => s.trim())
                .filter(Boolean)
            : [];

        return {
          id: name,
          label: labelText || name,
          constraints
        };
      })
      .filter(Boolean);

    if (datumFields.length === 0) {
      alert("Please add at least one datum field for your custom contract.");
      return null;
    }

    if (actions.length === 0) {
      alert("Please add at least one redeemer action for your custom contract.");
      return null;
    }

    return {
      id,
      label,
      datumType,
      redeemerType,
      datumFields,
      actions,
      constraints: [],
      defaultActionConstraints: {},
      isCustom: true,
      contractTitle: title
    };
  }

  // -------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------
  function capitalize(str) {
    if (!str) return "";
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  function getIndustryById(id) {
    for (let i = 0; i < industries.length; i++) {
      if (industries[i].id === id) {
        return industries[i];
      }
    }
    return null;
  }

  function populateIndustryDropdown() {
    for (let i = 0; i < industries.length; i++) {
      const ind = industries[i];
      const opt = document.createElement("option");
      opt.value = ind.id;
      opt.textContent = ind.label;
      industrySelect.appendChild(opt);
    }
  }

  function renderDatumFields(industry) {
    datumArea.innerHTML = "";
    for (let i = 0; i < industry.datumFields.length; i++) {
      const field = industry.datumFields[i];

      const label = document.createElement("label");
      const cb = document.createElement("input");
      cb.type = "checkbox";
      cb.name = "datum-field";
      cb.value = field.id;
      label.appendChild(cb);
      label.appendChild(document.createTextNode(" " + field.label));
      datumArea.appendChild(label);
    }
  }

  function renderActions(industry) {
    actionsArea.innerHTML = "";

    for (let i = 0; i < industry.actions.length; i++) {
      const action = industry.actions[i];
      const block  = document.createElement("div");
      block.className = "action-block";

      const title = document.createElement("h4");
      title.textContent = action.label;
      block.appendChild(title);

      const defaults = industry.defaultActionConstraints[action.id] || [];

      for (let j = 0; j < industry.constraints.length; j++) {
        const c = industry.constraints[j];

        const label = document.createElement("label");
        const cb = document.createElement("input");
        cb.type = "checkbox";
        cb.value = c.id;
        cb.name = "constraint-" + action.id;
        cb.checked = defaults.indexOf(c.id) !== -1;

        label.appendChild(cb);
        label.appendChild(document.createTextNode(" " + c.label));
        block.appendChild(label);
      }

      actionsArea.appendChild(block);
    }
  }

  function renderIndustry(id) {
    const industry = getIndustryById(id);
    if (!industry) return;

    renderDatumFields(industry);
    renderActions(industry);

    datumJsonOutput.value    = "";
    redeemerJsonOutput.value = "";
    if (haskellOutput)      haskellOutput.value = "";
    if (haskellFullOutput)  haskellFullOutput.value = "";
  }

  // -------------------------------------------------------------------
  // Build JSON specs
  // -------------------------------------------------------------------
  function buildDatumSpec(industry) {
    // Custom industry: use all defined fields directly
    if (industry && industry.isCustom) {
      const fields = (industry.datumFields || []).map(f => ({
        id: f.id,
        type: f.type,
        label: f.label
      }));
      return {
        datumType: industry.datumType || "CoxyDatum",
        fields,
        customFields: []
      };
    }

    // Normal industries: use checkboxes
    const checked = document.querySelectorAll('input[name="datum-field"]:checked');
    const fields  = [];

    for (let i = 0; i < checked.length; i++) {
      const cb = checked[i];
      let fieldDef = null;

      for (let j = 0; j < industry.datumFields.length; j++) {
        if (industry.datumFields[j].id === cb.value) {
          fieldDef = industry.datumFields[j];
          break;
        }
      }

      if (fieldDef) {
        fields.push({
          id: fieldDef.id,
          type: fieldDef.type,
          label: fieldDef.label
        });
      } else {
        fields.push({
          id: cb.value,
          type: "Unknown",
          label: cb.value
        });
      }
    }

    // Advanced custom datum fields (old toggle)
    const customFields = [];
    if (customDatumToggle && customDatumToggle.checked && customDatumSection) {
      const rows = customDatumSection.querySelectorAll(".custom-prop-row");
      for (let r = 0; r < rows.length; r++) {
        const row = rows[r];
        const inputs = row.querySelectorAll('input[type="text"]');
        if (inputs.length < 1) continue;
        const name = inputs[0].value.trim();
        const typ  = inputs.length > 1 ? inputs[1].value.trim() : "";
        if (!name) continue;
        customFields.push({
          name: name,
          type: typ || "Unknown"
        });
      }
    }

    return {
      datumType: industry.datumType || "CoxyDatum",
      fields: fields,
      customFields: customFields
    };
  }

  function buildRedeemerSpec(industry) {
    // Custom industry: use constraints defined per action (if any)
    if (industry && industry.isCustom) {
      const actionsSpec = (industry.actions || []).map(a => ({
        id: a.id,
        label: a.label,
        constraints: Array.isArray(a.constraints) ? a.constraints : []
      }));
      return {
        redeemerType: industry.redeemerType || "CoxyRedeemer",
        actions: actionsSpec,
        customActions: []
      };
    }

    // Normal industries
    const actionsSpec = [];

    for (let i = 0; i < industry.actions.length; i++) {
      const action = industry.actions[i];
      const selector = 'input[name="constraint-' + action.id + '"]:checked';
      const checked = document.querySelectorAll(selector);
      const constraints = [];

      for (let j = 0; j < checked.length; j++) {
        constraints.push(checked[j].value);
      }

      actionsSpec.push({
        id: action.id,
        label: action.label,
        constraints: constraints
      });
    }

    // Advanced custom redeemer actions (old toggle)
    const customActions = [];
    if (customRedeemerToggle && customRedeemerToggle.checked && customRedeemerSection) {
      const rows = customRedeemerSection.querySelectorAll(".custom-prop-row");
      for (let r = 0; r < rows.length; r++) {
        const row = rows[r];
        const inputs = row.querySelectorAll('input[type="text"]');
        if (!inputs.length) continue;
        const name = inputs[0].value.trim();
        const desc = inputs.length > 1 ? inputs[1].value.trim() : "";
        if (!name) continue;
        customActions.push({
          name: name,
          description: desc || null
        });
      }
    }

    return {
      redeemerType: industry.redeemerType || "CoxyRedeemer",
      actions: actionsSpec,
      customActions: customActions
    };
  }

  // -------------------------------------------------------------------
  // Haskell mkValidator preview (3rd box)
  // -------------------------------------------------------------------
  function mkHaskellPreview(datumSpec, redeemerSpec) {
    const dt = datumSpec.datumType        || "CoxyDatum";
    const rt = redeemerSpec.redeemerType || "CoxyRedeemer";

    const lines = [];
    lines.push("{-# INLINABLE mkValidator #-}");
    lines.push("mkValidator :: " + dt + " -> " + rt + " -> ScriptContext -> Bool");
    lines.push("mkValidator dat red ctx =");
    lines.push("  case red of");

    const actions    = (redeemerSpec && redeemerSpec.actions) ? redeemerSpec.actions : [];
    const customActs = (redeemerSpec && redeemerSpec.customActions) ? redeemerSpec.customActions : [];

    if (actions.length === 0 && customActs.length === 0) {
      lines.push("    _ -> True");
      return lines.join("\n");
    }

    for (let i = 0; i < actions.length; i++) {
      const action = actions[i];
      const cs = action.constraints || [];
      lines.push("    " + action.id + " ->");
      if (cs.length === 0) {
        lines.push("      True");
      } else {
        for (let j = 0; j < cs.length; j++) {
          const cid = cs[j];
          const prefix = (j === 0 ? "      " : "    && ");
          lines.push(
            prefix +
            'traceIfFalse "' +
            cid +
            '" (constraint_' +
            cid +
            " dat ctx)"
          );
        }
      }
    }

    for (let k = 0; k < customActs.length; k++) {
      const ca = customActs[k];
      if (!ca.name) continue;
      lines.push("    " + ca.name + " ->");
      lines.push('      traceIfFalse "customAction:' + ca.name + '" True');
    }

    return lines.join("\n");
  }

  // -------------------------------------------------------------------
  // Full Haskell module (4th box)
  // -------------------------------------------------------------------
  function mkFullHaskellModule(datumSpec, redeemerSpec) {
    const fields        = Array.isArray(datumSpec.fields) ? datumSpec.fields : [];
    const customFields  = Array.isArray(datumSpec.customFields) ? datumSpec.customFields : [];
    const actions       = Array.isArray(redeemerSpec.actions) ? redeemerSpec.actions : [];
    const customActions = Array.isArray(redeemerSpec.customActions) ? redeemerSpec.customActions : [];

    const dtName = datumSpec.datumType        || "CoxyDatum";
    const rtName = redeemerSpec.redeemerType  || "CoxyRedeemer";

    // Collect unique constraint IDs from actions
    const constraintIds = [];
    for (let i = 0; i < actions.length; i++) {
      const cs = actions[i].constraints || [];
      for (let j = 0; j < cs.length; j++) {
        const cid = cs[j];
        if (constraintIds.indexOf(cid) === -1) {
          constraintIds.push(cid);
        }
      }
    }

    const lines = [];

    // Pragmas
    lines.push("{-# LANGUAGE DataKinds           #-}");
    lines.push("{-# LANGUAGE NoImplicitPrelude   #-}");
    lines.push("{-# LANGUAGE TemplateHaskell     #-}");
    lines.push("{-# LANGUAGE ScopedTypeVariables #-}");
    lines.push("{-# LANGUAGE OverloadedStrings   #-}");
    lines.push("{-# LANGUAGE TypeApplications    #-}");
    lines.push("");
    // Module header
    lines.push("module ValidatorLogic");
    lines.push("  ( " + dtName);
    lines.push("  , " + rtName);
    lines.push("  , mkValidator");
    lines.push("  ) where");
    lines.push("");
    // Imports
    lines.push("import Plutus.V2.Ledger.Api");
    lines.push("import Plutus.V2.Ledger.Contexts");
    lines.push("import qualified Plutus.V2.Ledger.Api as PlutusV2");
    lines.push("import Plutus.V1.Ledger.Interval as Interval");
    lines.push("import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)");
    lines.push("import qualified PlutusTx as PlutusTx");
    lines.push("import PlutusTx.Prelude hiding (Semigroup(..), unless)");
    lines.push("import qualified PlutusTx.Builtins as Builtins");
    lines.push("import qualified Codec.Serialise as Serialise");
    lines.push("import qualified Data.ByteString.Lazy  as LBS");
    lines.push("import qualified Data.ByteString.Short as SBS");
    lines.push("");
    lines.push("------------------------------------------------------------------------");
    lines.push("-- Datum and Redeemer");
    lines.push("------------------------------------------------------------------------");
    lines.push("");

    // Datum
    if (fields.length === 0 && customFields.length === 0) {
      lines.push("data " + dtName + " = " + dtName);
    } else {
      const allFields = [];
      for (let f = 0; f < fields.length; f++) {
        allFields.push("cd" + capitalize(fields[f].id) + " :: " + fields[f].type);
      }
      for (let cf = 0; cf < customFields.length; cf++) {
        allFields.push("cd" + capitalize(customFields[cf].name) + " :: " + customFields[cf].type);
      }
      lines.push("data " + dtName + " = " + dtName);
      lines.push("    { " + allFields.join("\n    , ") + "\n    }");
    }
    lines.push("PlutusTx.unstableMakeIsData ''" + dtName);
    lines.push("");

    // Redeemer
    const constructors = [];
    for (let a = 0; a < actions.length; a++) {
      constructors.push(actions[a].id);
    }
    for (let ca = 0; ca < customActions.length; ca++) {
      if (customActions[ca].name) {
        constructors.push(customActions[ca].name);
      }
    }

    if (constructors.length === 0) {
      lines.push("data " + rtName + " = " + rtName);
    } else {
      lines.push("data " + rtName + " = " + constructors.join(" | "));
    }
    lines.push("PlutusTx.unstableMakeIsData ''" + rtName);
    lines.push("");

    // Validator
    lines.push("------------------------------------------------------------------------");
    lines.push("-- Validator Logic");
    lines.push("------------------------------------------------------------------------");
    lines.push("");
    lines.push("{-# INLINABLE mkValidator #-}");
    lines.push("mkValidator :: " + dtName + " -> " + rtName + " -> ScriptContext -> Bool");
    lines.push("mkValidator dat red ctx =");
    lines.push("  case red of");

    const hasStdActions = actions.length > 0;
    let hasCustomActs = false;
    for (let ca2 = 0; ca2 < customActions.length; ca2++) {
      if (customActions[ca2].name) {
        hasCustomActs = true;
        break;
      }
    }

    if (!hasStdActions && !hasCustomActs) {
      lines.push("    _ -> True");
    } else {
      // Standard actions with constraints
      for (let i2 = 0; i2 < actions.length; i2++) {
        const act = actions[i2];
        const cs2 = act.constraints || [];
        lines.push("    " + act.id + " ->");
        if (cs2.length === 0) {
          lines.push("      True");
        } else {
          for (let j2 = 0; j2 < cs2.length; j2++) {
            const cid2 = cs2[j2];
            const prefix2 = (j2 === 0 ? "      " : "      && ");
            lines.push(
              prefix2 +
              'traceIfFalse "' +
              cid2 +
              '" (' +
              "constraint_" +
              cid2 +
              " dat ctx)"
            );
          }
        }
      }

      // Custom actions: simple stub
      for (let k2 = 0; k2 < customActions.length; k2++) {
        const ca3 = customActions[k2];
        if (!ca3.name) continue;
        lines.push("    " + ca3.name + " ->");
        lines.push('      traceIfFalse "customAction:' + ca3.name + '" True');
      }
    }

    // Inline helper stubs in where block
    if (constraintIds.length > 0) {
      lines.push("  where");
      for (let c = 0; c < constraintIds.length; c++) {
        const cid3 = constraintIds[c];
        const fnName = "constraint_" + cid3;
        lines.push(
          "    " + fnName + " :: " + dtName + " -> ScriptContext -> Bool"
        );
        lines.push("    " + fnName + " _ _ = True");
        lines.push("\n");
      }
    }

    lines.push("");

    return lines.join("\n");
  }

  // -------------------------------------------------------------------
  // Advanced custom datum toggle
  // -------------------------------------------------------------------
  if (customDatumToggle && customDatumSection) {
    customDatumToggle.addEventListener("change", function () {
      const enabled = customDatumToggle.checked;
      customDatumSection.classList.toggle("hidden", !enabled);
      const inputs = customDatumSection.querySelectorAll('input[type="text"]');
      for (let i = 0; i < inputs.length; i++) {
        inputs[i].disabled = !enabled;
      }
    });
  }

  if (addCustomDatumBtn && customDatumList) {
    addCustomDatumBtn.addEventListener("click", function () {
      const row = document.createElement("div");
      row.className = "custom-prop-row";
      row.innerHTML =
        '<input type="text" placeholder="Field name" />' +
        '<input type="text" placeholder="Haskell type e.g. Integer" />';
      if (!customDatumToggle || !customDatumToggle.checked) {
        const ins = row.querySelectorAll("input");
        for (let i = 0; i < ins.length; i++) {
          ins[i].disabled = true;
        }
      }
      customDatumList.appendChild(row);
    });
  }

  // -------------------------------------------------------------------
  // Advanced custom redeemer toggle
  // -------------------------------------------------------------------
  if (customRedeemerToggle && customRedeemerSection) {
    customRedeemerToggle.addEventListener("change", function () {
      const enabled = customRedeemerToggle.checked;
      customRedeemerSection.classList.toggle("hidden", !enabled);
      const inputs = customRedeemerSection.querySelectorAll('input[type="text"]');
      for (let i = 0; i < inputs.length; i++) {
        inputs[i].disabled = !enabled;
      }
    });
  }

  if (addCustomRedeemerBtn && customRedeemerList) {
    addCustomRedeemerBtn.addEventListener("click", function () {
      const row = document.createElement("div");
      row.className = "custom-prop-row";
      row.innerHTML =
        '<input type="text" placeholder="Action name e.g. EmergencyClose" />' +
        '<input type="text" placeholder="Description (optional)" />';
      if (!customRedeemerToggle || !customRedeemerToggle.checked) {
        const ins = row.querySelectorAll("input");
        for (let i = 0; i < ins.length; i++) {
          ins[i].disabled = true;
        }
      }
      customRedeemerList.appendChild(row);
    });
  }

  // -------------------------------------------------------------------
  // Download ValidatorLogic.hs
  // -------------------------------------------------------------------
  if (downloadBtn) {
    downloadBtn.addEventListener("click", function () {
      if (!haskellFullOutput || !haskellFullOutput.value.trim()) {
        alert("Please click 'Generate Logic Spec' first to create ValidatorLogic.");
        return;
      }

      const content = haskellFullOutput.value;
      const blob = new Blob([content], {
        type: "text/plain;charset=utf-8"
      });
      const url = URL.createObjectURL(blob);

      const a = document.createElement("a");
      a.href = url;
      a.download = "ValidatorLogic.hs";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  // -------------------------------------------------------------------
  // Init + events
  // -------------------------------------------------------------------
  populateIndustryDropdown();

  const initialId = industries.length > 0 ? industries[0].id : null;
  if (initialId) {
    industrySelect.value = initialId;
    renderIndustry(initialId);
  }

  industrySelect.addEventListener("change", function () {
    renderIndustry(industrySelect.value);
  });

  // Generate Logic Spec (normal + custom industry)
  generateBtn.addEventListener("click", function () {
    let industry;

    if (enableCustomIndustry && enableCustomIndustry.checked) {
      industry = getCustomIndustryFromUI();
      if (!industry) return;
    } else {
      industry = getIndustryById(industrySelect.value);
      if (!industry) {
        datumJsonOutput.value = "";
        redeemerJsonOutput.value = "";
        if (haskellOutput)     haskellOutput.value = "";
        if (haskellFullOutput) haskellFullOutput.value = "";
        return;
      }
    }

    const datumSpec    = buildDatumSpec(industry);
    const redeemerSpec = buildRedeemerSpec(industry);

    datumJsonOutput.value    = JSON.stringify(datumSpec, null, 2);
    redeemerJsonOutput.value = JSON.stringify(redeemerSpec, null, 2);

    if (haskellOutput) {
      haskellOutput.value = mkHaskellPreview(datumSpec, redeemerSpec);
    }
    if (haskellFullOutput) {
      haskellFullOutput.value = mkFullHaskellModule(datumSpec, redeemerSpec);
    }
  });
});

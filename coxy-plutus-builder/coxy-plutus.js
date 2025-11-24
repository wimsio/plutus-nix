// coxypp.js (ES module)
import { industries } from "./coxy-industries.js";

window.addEventListener("DOMContentLoaded", () => {
  // -------------------------------------------------------------------
  // DOM refs
  // -------------------------------------------------------------------
  const industrySelect     = document.getElementById("industry");
  const datumArea          = document.getElementById("datum-area");
  const industrySearch     = document.getElementById("industry-search");
  const actionsArea        = document.getElementById("actions-area");
  const generateBtn        = document.getElementById("generate");

  const datumJsonOutput    = document.getElementById("datum-json-output");
  const redeemerJsonOutput = document.getElementById("redeemer-json-output");
  const haskellOutput      = document.getElementById("haskell-output");
  const haskellFullOutput  = document.getElementById("haskell-full-output");
  const downloadBtn        = document.getElementById("download-hs");

  // Advanced CoxyDatum
  const customDatumToggle  = document.getElementById("enable-custom-datum");
  const customDatumSection = document.getElementById("custom-datum-section");
  const customDatumList    = document.getElementById("custom-datum-list");
  const addCustomDatumBtn  = document.getElementById("add-custom-datum");

  // Advanced CoxyRedeemer
  const customRedeemerToggle  = document.getElementById("enable-custom-redeemer");
  const customRedeemerSection = document.getElementById("custom-redeemer-section");
  const customRedeemerList    = document.getElementById("custom-redeemer-list");
  const addCustomRedeemerBtn  = document.getElementById("add-custom-redeemer");

  // Param datum (CoxyParamDatum)
  const paramDatumToggle  = document.getElementById("enable-param-datum");
  const paramDatumSection = document.getElementById("param-datum-section");
  const paramDatumList    = document.getElementById("param-datum-list");
  const addParamDatumBtn  = document.getElementById("add-param-datum");

  // Custom industry UI
  const enableCustomIndustry       = document.getElementById("enable-custom-industry");
  const customIndustrySection      = document.getElementById("custom-industry-section");
  const customIndustryDatumList    = document.getElementById("custom-industry-datum-list");
  const addCustomIndustryDatumBtn  = document.getElementById("add-custom-industry-datum");
  const customIndustryActionsList  = document.getElementById("custom-industry-actions-list");
  const addCustomIndustryActionBtn = document.getElementById("add-custom-industry-action");
  const standardRow2               = document.getElementById("standard-row-2");

  // Tests output + copy/download buttons
  const plutusTestsOutput      = document.getElementById("plutus-tests-output");
  const formalTestsOutput      = document.getElementById("formal-tests-output");
  const downloadFormalBtn      = document.getElementById("download-formal-hs");
  const downloadTestsBtn       = document.getElementById("download-tests-hs");

  const copyHaskellFullBtn     = document.getElementById("copy-haskell-full-output");
  const copyPlutusTestsBtn     = document.getElementById("copy-plutus-tests-output");
  const copyFormalTestsBtn     = document.getElementById("copy-formal-tests-output");

  // -------------------------------------------------------------------
  // Shared Plutus type options (for custom selects)
  // -------------------------------------------------------------------
  const PLUTUS_TYPE_OPTIONS = `
    <option value="">Select Plutus type…</option>
    <option>Integer</option>
    <option>Bool</option>
    <option>BuiltinByteString</option>
    <option>POSIXTime</option>
    <option>POSIXTimeRange</option>
    <option>PubKeyHash</option>
    <option>ScriptHash</option>
    <option>CurrencySymbol</option>
    <option>TokenName</option>
    <option>Value</option>
    <option>AssetClass</option>
    <option>Address</option>
    <option>StakingCredential</option>
    <option>BuiltinData</option>
    <option>TxOutRef</option>
    <option>Maybe Integer</option>
    <option>Maybe PubKeyHash</option>
    <option>TxId</option>
    <option>TxInfo</option>
    <option>TxOut</option>
    <option>TxInInfo</option>
    <option>OutputDatum</option>
    <option>Maybe ScriptHash</option>
    <option>Lovelace</option>
    <option>Ada</option>
    <option>Interval</option>
    <option>Slot</option>
    <option>Time</option>
    <option>ValidatorHash</option>
    <option>LowerBound a</option>
    <option>UpperBound a</option>
    <option>Natural</option>
    <option>Credential</option>
    <option>StakingHash</option>
  `;

  // -------------------------------------------------------------------
  // Helpers
  // -------------------------------------------------------------------
  function capitalize(str) {
    if (!str) return "";
    return str.charAt(0).toUpperCase() + str.slice(1);
  }

  function hsIdentSafe(str) {
    return String(str || "")
      .replace(/[^A-Za-z0-9_]/g, "_")
      .replace(/^([0-9])/, "_$1");
  }

  function getIndustryById(id) {
    return industries.find(i => i.id === id) || null;
  }

  function clearOutputs() {
    if (datumJsonOutput)    datumJsonOutput.value    = "";
    if (redeemerJsonOutput) redeemerJsonOutput.value = "";
    if (haskellOutput)      haskellOutput.value      = "";
    if (haskellFullOutput)  haskellFullOutput.value  = "";
    if (formalTestsOutput)  formalTestsOutput.value  = "";
    if (plutusTestsOutput)  plutusTestsOutput.value  = "";
  }

  // -------------------------------------------------------------------
  // Custom industry factories
  // -------------------------------------------------------------------
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
        ${PLUTUS_TYPE_OPTIONS}
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
    `;
    return row;
  }

  // -------------------------------------------------------------------
  // Initial state (custom vs standard)
  // -------------------------------------------------------------------
  if (customIndustrySection) {
    customIndustrySection.classList.add("hidden");
  }
  if (standardRow2) {
    standardRow2.classList.remove("hidden");
  }

  if (enableCustomIndustry && customIndustrySection) {
    enableCustomIndustry.addEventListener("change", () => {
      const enabled = enableCustomIndustry.checked;

      if (plutusTestsOutput)  plutusTestsOutput.value  = "";
      if (formalTestsOutput)  formalTestsOutput.value  = "";

      // Toggle rows
      customIndustrySection.classList.toggle("hidden", !enabled);
      if (standardRow2) {
        standardRow2.classList.toggle("hidden", enabled);
      }

      // Lock / unlock template industry controls
      if (industrySelect) {
        industrySelect.disabled = enabled;
      }
      if (industrySearch) {
        industrySearch.disabled = enabled;
      }

      // Auto-toggle advanced datum/redeemer
      if (customDatumToggle) {
        customDatumToggle.checked = enabled;
        customDatumToggle.dispatchEvent(new Event("change"));
      }
      if (customRedeemerToggle) {
        customRedeemerToggle.checked = enabled;
        customRedeemerToggle.dispatchEvent(new Event("change"));
      }

      // Clear UI panels
      if (datumArea)   datumArea.innerHTML   = "";
      if (actionsArea) actionsArea.innerHTML = "";

      // Back to standard templates
      if (!enabled) {
        if (industrySearch) {
          industrySearch.value = "";
        }
        populateIndustryDropdown();
        if (industrySelect && industrySelect.value) {
          renderIndustry(industrySelect.value);
        }
      }

      clearOutputs();
    });
  }

  if (addCustomIndustryDatumBtn && customIndustryDatumList) {
    addCustomIndustryDatumBtn.addEventListener("click", () => {
      customIndustryDatumList.appendChild(createCustomIndustryDatumRow());
    });
  }

  if (addCustomIndustryActionBtn && customIndustryActionsList) {
    addCustomIndustryActionBtn.addEventListener("click", () => {
      customIndustryActionsList.appendChild(createCustomIndustryActionRow());
    });
  }

  // -------------------------------------------------------------------
  // Custom industry builder
  // -------------------------------------------------------------------
function getCustomIndustryFromUI() {
  const titleInput        = document.getElementById("custom-contract-title");
  const customIdInput     = document.getElementById("custom-industry-id");
  const customLabelInput  = document.getElementById("custom-industry-label");
  const datumTypeInput    = document.getElementById("custom-datum-type");
  const redeemerTypeInput = document.getElementById("custom-redeemer-type");

  const title        = (titleInput?.value || "").trim() || "CustomContract";
  const id           = (customIdInput?.value || "").trim() || "customContract";
  const label        = (customLabelInput?.value || "").trim() || title;
  const datumType    = (datumTypeInput?.value || "").trim() || "CoxyDatum";
  const redeemerType = (redeemerTypeInput?.value || "").trim() || "CoxyRedeemer";

  // ---------------------------------------------------------------
  // DATUM FIELDS: custom-industry + advanced CoxyDatum
  // ---------------------------------------------------------------
  const datumRows = Array.from(
    customIndustryDatumList?.querySelectorAll(".custom-industry-datum-row") || []
  );

  const baseDatumFields = datumRows
    .map(row => {
      const nameEl = row.querySelector(".cid-name");
      const typeEl = row.querySelector(".cid-type");
      const name   = (nameEl?.value || "").trim();
      const type   = (typeEl?.value || "").trim();
      if (!name || !type) return null;
      return { id: name, label: `${name} :: ${type}`, type };
    })
    .filter(Boolean);

  const extraDatumFields = [];
  if (customDatumList && customDatumToggle && customDatumToggle.checked) {
    const advRows = customDatumList.querySelectorAll(".custom-prop-row");
    advRows.forEach(row => {
      const nameEl = row.querySelector(".custom-datum-name");
      const typeEl = row.querySelector(".custom-datum-type");
      const name   = (nameEl?.value || "").trim();
      const type   = (typeEl?.value || "").trim();
      if (!name || !type) return;
      extraDatumFields.push({
        id: name,
        label: `${name} :: ${type}`,
        type
      });
    });
  }

  let datumFields = [...baseDatumFields, ...extraDatumFields];

  // If user hasn't defined any datum fields, seed a default one
  if (!datumFields.length) {
    datumFields = [
      { id: "fieldOne", label: "fieldOne :: PubKeyHash", type: "PubKeyHash" }
    ];

    // Also show it in the UI if nothing is there yet
    if (customIndustryDatumList && !customIndustryDatumList.children.length) {
      const row = createCustomIndustryDatumRow();
      const nameEl = row.querySelector(".cid-name");
      const typeEl = row.querySelector(".cid-type");
      if (nameEl) nameEl.value = "fieldOne";
      if (typeEl) typeEl.value = "PubKeyHash";
      customIndustryDatumList.appendChild(row);
    }
  }

  // ---------------------------------------------------------------
  // REDEEMER ACTIONS: custom-industry + advanced actions
  // ---------------------------------------------------------------
  const actionRows = Array.from(
    customIndustryActionsList?.querySelectorAll(".custom-industry-action-row") || []
  );

  const baseActions = actionRows
    .map(row => {
      const nameEl  = row.querySelector(".cia-name");
      const labelEl = row.querySelector(".cia-label");
      const name      = (nameEl?.value || "").trim();
      const labelText = (labelEl?.value || "").trim();
      if (!name) return null;
      return { id: name, label: labelText || name };
    })
    .filter(Boolean);

  const extraActions = [];
  if (customRedeemerList && customRedeemerToggle && customRedeemerToggle.checked) {
    const advRows = customRedeemerList.querySelectorAll(".custom-prop-row");
    advRows.forEach(row => {
      const inputs = row.querySelectorAll('input[type="text"]');
      if (!inputs.length) return;
      const name = (inputs[0].value || "").trim();
      const desc = inputs.length > 1 ? (inputs[1].value || "").trim() : "";
      if (!name) return;
      extraActions.push({
        id: name,
        label: desc || name
      });
    });
  }

  let actions = [...baseActions, ...extraActions];

  // If user hasn't defined any actions, seed the demo one
  if (!actions.length) {
    actions = [
      { id: "ActionOne", label: "First Action of Demo Redeemer" }
    ];

    // Also reflect this in the UI if there are no rows yet
    if (customIndustryActionsList && !customIndustryActionsList.children.length) {
      const row = createCustomIndustryActionRow();
      const nameEl  = row.querySelector(".cia-name");
      const labelEl = row.querySelector(".cia-label");
      if (nameEl)  nameEl.value  = "ActionOne";
      if (labelEl) labelEl.value = "First Action of Demo Redeemer";
      customIndustryActionsList.appendChild(row);
    }
  }

  // ---------------------------------------------------------------
  // Return custom industry descriptor
  // ---------------------------------------------------------------
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
  // Industry dropdown + rendering
  // -------------------------------------------------------------------
  function populateIndustryDropdown(filterText = "") {
    if (!industrySelect) return;

    const previousSelection = industrySelect.value;
    const term = (filterText || "").trim().toLowerCase();

    const filtered = term
      ? industries.filter(ind =>
          ind.label.toLowerCase().includes(term) ||
          ind.id.toLowerCase().includes(term)
        )
      : industries.slice();

    industrySelect.innerHTML = "";

    if (filtered.length === 0) {
      const opt = document.createElement("option");
      opt.value = "";
      opt.textContent = `No results for "${filterText}"`;
      opt.disabled = true;
      industrySelect.appendChild(opt);

      if (datumArea)   datumArea.innerHTML   = "";
      if (actionsArea) actionsArea.innerHTML = "";
      clearOutputs();
      return;
    }

    filtered.forEach(ind => {
      const opt = document.createElement("option");
      opt.value = ind.id;
      opt.textContent = ind.label;
      industrySelect.appendChild(opt);
    });

    const stillExists = filtered.some(ind => ind.id === previousSelection);
    industrySelect.value = stillExists ? previousSelection : filtered[0].id;
    renderIndustry(industrySelect.value);
  }

  function renderDatumFields(industry) {
    if (!datumArea) return;
    datumArea.innerHTML = "";
    industry.datumFields.forEach(field => {
      const label = document.createElement("label");
      const cb    = document.createElement("input");
      cb.type  = "checkbox";
      cb.name  = "datum-field";
      cb.value = field.id;
      label.appendChild(cb);
      label.appendChild(document.createTextNode(" " + field.label));
      datumArea.appendChild(label);
    });
  }

  function renderActions(industry) {
    if (!actionsArea) return;
    actionsArea.innerHTML = "";
    industry.actions.forEach(action => {
      const block  = document.createElement("div");
      block.className = "action-block";

      const title = document.createElement("h4");
      title.textContent = action.label;
      block.appendChild(title);

      const defaults = industry.defaultActionConstraints[action.id] || [];

      industry.constraints.forEach(c => {
        const label = document.createElement("label");
        const cb    = document.createElement("input");
        cb.type    = "checkbox";
        cb.value   = c.id;
        cb.name    = "constraint-" + action.id;
        cb.checked = defaults.includes(c.id);
        label.appendChild(cb);
        label.appendChild(document.createTextNode(" " + c.label));
        block.appendChild(label);
      });

      actionsArea.appendChild(block);
    });
  }

  function renderIndustry(id) {
    const industry = getIndustryById(id);
    if (!industry) return;
    renderDatumFields(industry);
    renderActions(industry);
    clearOutputs();
  }

  // -------------------------------------------------------------------
  // Datum spec (CoxyDatum)
  // -------------------------------------------------------------------
  function buildDatumSpec(industry) {
    if (industry && industry.isCustom) {
      const fields = (industry.datumFields || []).map(f => ({
        id: f.id,
        type: f.type,
        label: f.label
      }));
      return {
        datumType: "CoxyDatum",
        fields,
        customFields: []
      };
    }

    const checked = document.querySelectorAll('input[name="datum-field"]:checked');
    const fields  = [];

    checked.forEach(cb => {
      const fieldDef = industry.datumFields.find(f => f.id === cb.value);
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
    });

    const customFields = [];
    if (customDatumToggle && customDatumToggle.checked && customDatumSection) {
      const rows = customDatumSection.querySelectorAll(".custom-prop-row");
      rows.forEach(row => {
        const nameEl = row.querySelector(".custom-datum-name");
        const typeEl = row.querySelector(".custom-datum-type");
        if (!nameEl || !typeEl) return;
        const name = nameEl.value.trim();
        const typ  = typeEl.value.trim();
        if (!name || !typ) return;
        customFields.push({ name, type: typ });
      });
    }

    return {
      datumType: "CoxyDatum",
      fields,
      customFields
    };
  }

  // -------------------------------------------------------------------
  // Param datum spec (CoxyParamDatum)
  // -------------------------------------------------------------------
  function buildParamDatumSpec() {
    if (!paramDatumToggle || !paramDatumToggle.checked) return null;
    if (!paramDatumList) return null;

    const rows = paramDatumList.querySelectorAll(".param-datum-row");
    const fields = [];

    rows.forEach(row => {
      const nameEl = row.querySelector(".param-name");
      const typeEl = row.querySelector(".param-type");
      if (!nameEl || !typeEl) return;
      const name = nameEl.value.trim();
      const typ  = typeEl.value.trim();
      if (!name || !typ) return;
      fields.push({
        id: name,
        type: typ,
        label: `${name} :: ${typ}`
      });
    });

    if (!fields.length) {
      alert(
        "You enabled parameter datum, but no fields are defined.\n" +
        "Either add at least one CoxyParamDatum field or disable the parameter datum option."
      );
      return null;
    }

    return {
      datumType: "CoxyParamDatum",
      fields,
      customFields: []
    };
  }

  // -------------------------------------------------------------------
  // Redeemer spec
  // -------------------------------------------------------------------
  function buildRedeemerSpec(industry) {
    if (industry && industry.isCustom) {
      const actionsSpec = (industry.actions || []).map(a => ({
        id: a.id,
        label: a.label,
        constraints: []
      }));
      return {
        redeemerType: "CoxyRedeemer",
        actions: actionsSpec,
        customActions: []
      };
    }

    const actionsSpec = [];
    industry.actions.forEach(action => {
      const selector  = `input[name="constraint-${action.id}"]:checked`;
      const checked   = document.querySelectorAll(selector);
      const cs        = [];
      checked.forEach(cb => cs.push(cb.value));

      actionsSpec.push({
        id: action.id,
        label: action.label,
        constraints: cs
      });
    });

    const customActions = [];
    if (customRedeemerToggle && customRedeemerToggle.checked && customRedeemerSection) {
      const rows = customRedeemerSection.querySelectorAll(".custom-prop-row");
      rows.forEach(row => {
        const inputs = row.querySelectorAll('input[type="text"]');
        if (!inputs.length) return;
        const name = inputs[0].value.trim();
        const desc = inputs.length > 1 ? inputs[1].value.trim() : "";
        if (!name) return;
        customActions.push({
          name,
          description: desc || null
        });
      });
    }

    return {
      redeemerType: "CoxyRedeemer",
      actions: actionsSpec,
      customActions
    };
  }

  // -------------------------------------------------------------------
  // Haskell mkValidator preview
  // -------------------------------------------------------------------
  function mkHaskellPreview(datumSpec, redeemerSpec, paramDatumSpec) {
    const dtName  = "CoxyDatum";
    const rtName  = "CoxyRedeemer";
    const paramDt = paramDatumSpec ? "CoxyParamDatum" : null;

    const paramTypePart = paramDt ? paramDt + " -> " : "";
    const paramArg      = paramDt ? "pdat " : "";

    const lines = [];
    lines.push("{-# INLINABLE mkValidator #-}");
    lines.push(
      "mkValidator :: " +
        paramTypePart +
        dtName +
        " -> " +
        rtName +
        " -> ScriptContext -> Bool"
    );
    lines.push("mkValidator " + paramArg + "dat red ctx =");
    lines.push("  case red of");

    const actions    = Array.isArray(redeemerSpec.actions) ? redeemerSpec.actions : [];
    const customActs = Array.isArray(redeemerSpec.customActions) ? redeemerSpec.customActions : [];

    if (!actions.length && !customActs.length) {
      lines.push("    _ -> True");
      return lines.join("\n");
    }

    actions.forEach(action => {
      const cs = action.constraints || [];
      lines.push("    " + action.id + " ->");
      if (!cs.length) {
        lines.push("      True");
      } else {
        cs.forEach((cid, idx) => {
          const prefix = idx === 0 ? "      " : "    && ";
          lines.push(
            prefix +
              'traceIfFalse "' +
              cid +
              '" (constraint_' +
              cid +
              " dat ctx)"
          );
        });
      }
    });

    customActs.forEach(ca => {
      if (!ca.name) return;
      lines.push("    " + ca.name + " ->");
      lines.push('      traceIfFalse "customAction:' + ca.name + '" True');
    });

    return lines.join("\n");
  }

  // -------------------------------------------------------------------
  // Full Haskell module (ValidatorLogic.hs)
  // -------------------------------------------------------------------
  function mkFullHaskellModule(datumSpec, redeemerSpec, paramDatumSpec) {
    const fields        = Array.isArray(datumSpec.fields) ? datumSpec.fields : [];
    const customFields  = Array.isArray(datumSpec.customFields) ? datumSpec.customFields : [];
    const actions       = Array.isArray(redeemerSpec.actions) ? redeemerSpec.actions : [];
    const customActions = Array.isArray(redeemerSpec.customActions) ? redeemerSpec.customActions : [];

    const dtName  = "CoxyDatum";
    const rtName  = "CoxyRedeemer";
    const paramDt = paramDatumSpec ? "CoxyParamDatum" : null;

    const paramFields =
      paramDatumSpec && Array.isArray(paramDatumSpec.fields)
        ? paramDatumSpec.fields
        : [];
    const paramCustomFields =
      paramDatumSpec && Array.isArray(paramDatumSpec.customFields)
        ? paramDatumSpec.customFields
        : [];

    const constraintIds = [];
    actions.forEach(a => {
      (a.constraints || []).forEach(cid => {
        if (!constraintIds.includes(cid)) constraintIds.push(cid);
      });
    });

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
    if (paramDt) {
      lines.push("  , " + paramDt);
    }
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

    // Param datum
    if (paramDt) {
      if (!paramFields.length && !paramCustomFields.length) {
        lines.push("data " + paramDt + " = " + paramDt);
      } else {
        const allParamFields = [];
        paramFields.forEach(f => {
          allParamFields.push("cp" + capitalize(f.id) + " :: " + f.type);
        });
        paramCustomFields.forEach(cf => {
          allParamFields.push("cp" + capitalize(cf.name) + " :: " + cf.type);
        });
        lines.push("data " + paramDt + " = " + paramDt);
        lines.push("    { " + allParamFields.join("\n    , ") + "\n    }");
      }
      lines.push("PlutusTx.unstableMakeIsData ''" + paramDt);
      lines.push("");
    }

    // CoxyDatum
    if (!fields.length && !customFields.length) {
      lines.push("data " + dtName + " = " + dtName);
    } else {
      const allFields = [];
      fields.forEach(f => {
        allFields.push("cd" + capitalize(f.id) + " :: " + f.type);
      });
      customFields.forEach(cf => {
        allFields.push("cd" + capitalize(cf.name) + " :: " + cf.type);
      });
      lines.push("data " + dtName + " = " + dtName);
      lines.push("    { " + allFields.join("\n    , ") + "\n    }");
    }
    lines.push("PlutusTx.unstableMakeIsData ''" + dtName);
    lines.push("");

    // CoxyRedeemer
    const constructors = [];
    actions.forEach(a => constructors.push(a.id));
    customActions.forEach(ca => {
      if (ca.name) constructors.push(ca.name);
    });

    if (!constructors.length) {
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

    const paramTypePart = paramDt ? paramDt + " -> " : "";
    const paramArg      = paramDt ? "pdat " : "";

    lines.push("{-# INLINABLE mkValidator #-}");
    lines.push(
      "mkValidator :: " +
        paramTypePart +
        dtName +
        " -> " +
        rtName +
        " -> ScriptContext -> Bool"
    );
    lines.push("mkValidator " + paramArg + "dat red ctx =");
    lines.push("  case red of");

    const hasStdActions = actions.length > 0;
    const hasCustomActs = customActions.some(ca => ca.name);

    if (!hasStdActions && !hasCustomActs) {
      lines.push("    _ -> True");
    } else {
      actions.forEach(act => {
        const cs = act.constraints || [];
        lines.push("    " + act.id + " ->");
        if (!cs.length) {
          lines.push("      True");
        } else {
          cs.forEach((cid, idx) => {
            const prefix = idx === 0 ? "      " : "      && ";
            lines.push(
              prefix +
                'traceIfFalse "' +
                cid +
                '" (' +
                "constraint_" +
                cid +
                " dat ctx)"
            );
          });
        }
      });

      customActions.forEach(ca => {
        if (!ca.name) return;
        lines.push("    " + ca.name + " ->");
        lines.push('      traceIfFalse "customAction:' + ca.name + '" True');
      });
    }

    if (constraintIds.length > 0) {
      lines.push("  where");
      constraintIds.forEach(cid => {
        const fnName = "constraint_" + cid;
        lines.push("    " + fnName + " :: " + dtName + " -> ScriptContext -> Bool");
        lines.push("    " + fnName + " _ _ = True");
        lines.push("");
      });
    }

    lines.push("");
    return lines.join("\n");
  }

  // -------------------------------------------------------------------
  // Column 2: ValidatorLogicSpec.hs (lightweight unit skeleton)
  // -------------------------------------------------------------------
  function mkValidatorLogicSpecModule(datumSpec, redeemerSpec, paramDatumSpec) {
    const dtName   = (datumSpec?.datumType)    || "CoxyDatum";
    const rtName   = (redeemerSpec?.redeemerType) || "CoxyRedeemer";
    const hasParam = !!paramDatumSpec;

    const actions = Array.isArray(redeemerSpec?.actions) ? redeemerSpec.actions : [];
    const lines = [];

    lines.push("-- GENERATED: ValidatorLogicSpec (lightweight unit skeleton)");
    lines.push("{-# LANGUAGE OverloadedStrings #-}");
    lines.push("");
    lines.push("module ValidatorLogicSpec");
    lines.push("  ( tests");
    lines.push("  ) where");
    lines.push("");
    lines.push("import Test.Tasty");
    lines.push("import Test.Tasty.HUnit");
    lines.push("");
    lines.push("import Plutus.V2.Ledger.Api (ScriptContext)");
    lines.push("import ValidatorLogic");
    lines.push(
      "  ( " + [
        hasParam ? "CoxyParamDatum" : null,
        dtName,
        rtName + "(..)",
        "mkValidator"
      ].filter(Boolean).join(", ") + " )"
    );
    lines.push("");
    if (hasParam) {
      lines.push("dummyParam :: CoxyParamDatum");
      lines.push('dummyParam = error "TODO: construct CoxyParamDatum"');
      lines.push("");
    }
    lines.push("dummyDatum :: " + dtName);
    lines.push('dummyDatum = error "TODO: construct ' + dtName + '"');
    lines.push("");
    lines.push("dummyCtx :: ScriptContext");
    lines.push('dummyCtx = error "TODO: construct ScriptContext"');
    lines.push("");
    lines.push("-- Lightweight, “does it link & shape exist?” tests");

    const testFns = [];
    if (actions.length === 0) {
      lines.push("prop_noActions :: Assertion");
      lines.push('prop_noActions = assertBool "No actions (skeleton)" True');
      testFns.push({ label: "No actions (skeleton)", name: "prop_noActions" });
    } else {
      actions.forEach(a => {
        const cs = Array.isArray(a.constraints) && a.constraints.length ? a.constraints : [null];
        cs.forEach(c => {
          const label = c ? `${a.id} + ${c}` : `${a.id} (skeleton)`;
          const fn    = `prop_${hsIdentSafe(a.id)}${c ? "_" + hsIdentSafe(c) : ""}_holds`;
          lines.push(`${fn} :: Assertion`);
          lines.push(`${fn} = assertBool "${label}" True`);
          lines.push("");
          testFns.push({ label, name: fn });
        });
      });
    }

    lines.push("tests :: TestTree");
    lines.push('tests = testGroup "ValidatorLogic properties (skeleton)"');
    lines.push("  [");
    lines.push(
      testFns
        .map((t, i) =>
          `    testCase "${t.label}" ${t.name}${i === testFns.length - 1 ? "" : ","}`
        )
        .join("\n")
    );
    lines.push("  ]");
    return lines.join("\n");
  }

  // -------------------------------------------------------------------
  // Column 3 helpers: all-constraint map for formal module
  // -------------------------------------------------------------------
  function buildAllConstraintMapForFormal(industry, redeemerSpec) {
    const allIds = (industry?.constraints || []).map(c => c.id);
    const map = {};
    (redeemerSpec?.actions || []).forEach(a => {
      map[a.id] = allIds.length ? allIds.slice() : [];
    });
    return map;
  }

  // -------------------------------------------------------------------
  // Column 3: ValidatorLogicFormalSpec.hs (formal skeleton)
  // -------------------------------------------------------------------
  function mkFormalProofModule(datumSpec, redeemerSpec, paramDatumSpec, allConstraintIdsPerAction) {
    const dtName = (datumSpec?.datumType)    || "CoxyDatum";
    const rtName = (redeemerSpec?.redeemerType) || "CoxyRedeemer";
    const actions = Array.isArray(redeemerSpec?.actions) ? redeemerSpec.actions : [];

    const L = [];
    L.push("-- GENERATED: ValidatorLogicFormalSpec (formal skeleton)");
    L.push("{-# LANGUAGE OverloadedStrings #-}");
    L.push("");
    L.push("module ValidatorLogicFormalSpec");
    L.push("  ( tests");
    L.push("  ) where");
    L.push("");
    L.push("import Test.Tasty");
    L.push("import Test.Tasty.HUnit");
    L.push("");
    L.push("import Plutus.V2.Ledger.Api (ScriptContext)");
    L.push(`import ValidatorLogic ( ${dtName}, ${rtName}(..), mkValidator )`);
    L.push("");
    L.push("-- Dummy placeholders; refine with emulator-based tests and generators.");
    L.push(`dummyDatum :: ${dtName}`);
    L.push(`dummyDatum = error "TODO: construct ${dtName}"`);
    L.push("");
    L.push("dummyCtx :: ScriptContext");
    L.push('dummyCtx = error "TODO: construct ScriptContext"');
    L.push("");
    L.push("-- Project-wide invariant (refine): e.g. script UTxO must carry policy NFT");
    L.push(`invariant_scriptNFT :: ${dtName} -> ScriptContext -> Bool`);
    L.push("invariant_scriptNFT _ _ = True");
    L.push("");

    const testDefs = [];
    const testCases = [];

    if (!actions.length) {
      L.push("tests :: TestTree");
      L.push('tests = testGroup "ValidatorLogic formal skeleton" []');
      return L.join("\n");
    }

    actions.forEach(a => {
      const act = String(a.id || "").trim();
      if (!act) return;

      const allIds = Array.isArray(allConstraintIdsPerAction?.[act]) &&
                     allConstraintIdsPerAction[act].length
        ? allConstraintIdsPerAction[act]
        : (Array.isArray(a.constraints) && a.constraints.length ? a.constraints : [null]);

      allIds.forEach(cid => {
        const tag   = cid ? `${act}_${cid}` : act;
        const label = cid ? `${act} + ${cid}` : `${act}`;
        const pre   = `pre_${tag}`;
        const post  = `post_${tag}`;
        const prop  = `prop_${tag}`;

        testDefs.push(
          `${pre} :: ${dtName} -> ScriptContext -> Bool`,
          `${pre} _ _ = True`,
          "",
          `${post} :: ${dtName} -> ScriptContext -> Bool`,
          `${post} _ _ = True`,
          "",
          `${prop} :: Assertion`,
          `${prop} = do`,
          `  -- Hoare-style intent:`,
          `  --   { invariant_scriptNFT dat ctx && ${pre} dat ctx }`,
          `  --     mkValidator dat ${act} ctx`,
          `  --   { ${post} dat ctx }`,
          `  -- Replace the 'True' below with a real implication once helpers exist.`,
          `  assertBool "${label} formal skeleton" True`,
          ""
        );

        testCases.push(`      testCase "${label} formal skeleton" ${prop}`);
      });
    });

    L.push(testDefs.join("\n"));
    L.push("tests :: TestTree");
    L.push('tests = testGroup "ValidatorLogic formal skeleton"');
    L.push("  [");
    L.push(testCases.join(",\n"));
    L.push("  ]");

    return L.join("\n");
  }

  // -------------------------------------------------------------------
  // Toggles & add-row buttons
  // -------------------------------------------------------------------
  if (customDatumToggle && customDatumSection) {
    customDatumToggle.addEventListener("change", () => {
      const enabled = customDatumToggle.checked;
      customDatumSection.classList.toggle("hidden", !enabled);

      const inputs = customDatumSection.querySelectorAll('input[type="text"]');
      inputs.forEach(inp => { inp.disabled = !enabled; });

      const selects = customDatumSection.querySelectorAll("select");
      selects.forEach(sel => { sel.disabled = !enabled; });
    });
  }

  if (customRedeemerToggle && customRedeemerSection) {
    customRedeemerToggle.addEventListener("change", () => {
      const enabled = customRedeemerToggle.checked;
      customRedeemerSection.classList.toggle("hidden", !enabled);
      const inputs = customRedeemerSection.querySelectorAll('input[type="text"]');
      inputs.forEach(inp => { inp.disabled = !enabled; });
    });
  }

  if (paramDatumToggle && paramDatumSection) {
    paramDatumToggle.addEventListener("change", () => {
      const enabled = paramDatumToggle.checked;
      paramDatumSection.classList.toggle("hidden", !enabled);
      const inputs  = paramDatumSection.querySelectorAll('input[type="text"]');
      const selects = paramDatumSection.querySelectorAll("select");
      inputs.forEach(inp => { inp.disabled = !enabled; });
      selects.forEach(sel => { sel.disabled = !enabled; });
    });
  }

  if (addCustomDatumBtn && customDatumList) {
    addCustomDatumBtn.addEventListener("click", () => {
      const row = document.createElement("div");
      row.className = "custom-prop-row";
      row.innerHTML = `
        <input
          type="text"
          class="custom-datum-name"
          placeholder="Field name e.g. extraInfo"
        />
        <select class="custom-datum-type">
          ${PLUTUS_TYPE_OPTIONS}
        </select>
      `;
      if (!customDatumToggle || !customDatumToggle.checked) {
        row.querySelectorAll("input, select").forEach(inp => { inp.disabled = true; });
      }
      customDatumList.appendChild(row);
    });
  }

  if (addCustomRedeemerBtn && customRedeemerList) {
    addCustomRedeemerBtn.addEventListener("click", () => {
      const row = document.createElement("div");
      row.className = "custom-prop-row";
      row.innerHTML =
        '<input type="text" placeholder="Action name e.g. EmergencyClose" />' +
        '<input type="text" placeholder="Description (optional)" />';
      if (!customRedeemerToggle || !customRedeemerToggle.checked) {
        row.querySelectorAll("input").forEach(inp => { inp.disabled = true; });
      }
      customRedeemerList.appendChild(row);
    });
  }

  if (addParamDatumBtn && paramDatumList) {
    addParamDatumBtn.addEventListener("click", () => {
      const row = document.createElement("div");
      row.className = "custom-prop-row param-datum-row";
      row.innerHTML = `
        <input
          type="text"
          class="param-name"
          placeholder="Field name e.g. buyer"
        />
        <select class="param-type">
          ${PLUTUS_TYPE_OPTIONS}
        </select>
      `;
      if (!paramDatumToggle || !paramDatumToggle.checked) {
        row.querySelectorAll("input, select").forEach(inp => { inp.disabled = true; });
      }
      paramDatumList.appendChild(row);
    });
  }

  // -------------------------------------------------------------------
  // Download buttons
  // -------------------------------------------------------------------
  if (downloadBtn && haskellFullOutput) {
    downloadBtn.addEventListener("click", () => {
      if (!haskellFullOutput.value.trim()) {
        alert("Please click 'Generate Logic Spec' first to create ValidatorLogic.hs.");
        return;
      }
      const content = haskellFullOutput.value;
      const blob    = new Blob([content], { type: "text/plain;charset=utf-8" });
      const url     = URL.createObjectURL(blob);

      const a = document.createElement("a");
      a.href     = url;
      a.download = "ValidatorLogic.hs";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  if (downloadTestsBtn && plutusTestsOutput) {
    downloadTestsBtn.addEventListener("click", () => {
      if (!plutusTestsOutput.value.trim()) {
        alert("Please click 'Generate Logic Spec' first to create ValidatorLogicSpec.hs.");
        return;
      }
      const blob = new Blob([plutusTestsOutput.value], { type: "text/plain;charset=utf-8" });
      const url  = URL.createObjectURL(blob);
      const a    = document.createElement("a");
      a.href = url;
      a.download = "ValidatorLogicSpec.hs";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  if (downloadFormalBtn && formalTestsOutput) {
    downloadFormalBtn.addEventListener("click", () => {
      if (!formalTestsOutput.value.trim()) {
        alert("Please click 'Generate Logic Spec' first to create ValidatorLogicFormalSpec.hs.");
        return;
      }
      const content = formalTestsOutput.value;
      const blob    = new Blob([content], { type: "text/plain;charset=utf-8" });
      const url     = URL.createObjectURL(blob);

      const a = document.createElement("a");
      a.href     = url;
      a.download = "ValidatorLogicFormalSpec.hs";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      URL.revokeObjectURL(url);
    });
  }

  // -------------------------------------------------------------------
  // Copy buttons (guarded so they don't throw if missing)
  // -------------------------------------------------------------------
  if (copyHaskellFullBtn && haskellFullOutput) {
    copyHaskellFullBtn.addEventListener("click", async () => {
      const textarea = haskellFullOutput;
      const button   = copyHaskellFullBtn;

      textarea.select();
      textarea.setSelectionRange(0, textarea.value.length);

      try {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(textarea.value);
        } else {
          document.execCommand("copy");
        }
        const originalText = button.textContent;
        button.textContent = "Copied!";
        setTimeout(() => (button.textContent = originalText), 1500);
      } catch (err) {
        console.error("Copy failed:", err);
        alert("Could not copy to clipboard");
      }
    });
  }

  if (copyPlutusTestsBtn && plutusTestsOutput) {
    copyPlutusTestsBtn.addEventListener("click", async () => {
      const textarea = plutusTestsOutput;
      const button   = copyPlutusTestsBtn;

      textarea.select();
      textarea.setSelectionRange(0, textarea.value.length);

      try {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(textarea.value);
        } else {
          document.execCommand("copy");
        }
        const originalText = button.textContent;
        button.textContent = "Copied!";
        setTimeout(() => (button.textContent = originalText), 1500);
      } catch (err) {
        console.error("Copy failed:", err);
        alert("Could not copy to clipboard");
      }
    });
  }

  if (copyFormalTestsBtn && formalTestsOutput) {
    copyFormalTestsBtn.addEventListener("click", async () => {
      const textarea = formalTestsOutput;
      const button   = copyFormalTestsBtn;

      textarea.select();
      textarea.setSelectionRange(0, textarea.value.length);

      try {
        if (navigator.clipboard && navigator.clipboard.writeText) {
          await navigator.clipboard.writeText(textarea.value);
        } else {
          document.execCommand("copy");
        }
        const originalText = button.textContent;
        button.textContent = "Copied!";
        setTimeout(() => (button.textContent = originalText), 1500);
      } catch (err) {
        console.error("Copy failed:", err);
        alert("Could not copy to clipboard");
      }
    });
  }

  // -------------------------------------------------------------------
  // Initial populate + search
  // -------------------------------------------------------------------
  populateIndustryDropdown();

  const initialId = industries.length > 0 ? industries[0].id : null;
  if (initialId && industrySelect) {
    industrySelect.value = initialId;
    renderIndustry(initialId);
  }

  if (industrySelect) {
    industrySelect.addEventListener("change", () => {
      renderIndustry(industrySelect.value);
    });
  }

  if (industrySearch) {
    industrySearch.addEventListener("input", () => {
      populateIndustryDropdown(industrySearch.value);
    });
  }

  // -------------------------------------------------------------------
  // Generate button handler
  // -------------------------------------------------------------------
  if (generateBtn) {
    generateBtn.addEventListener("click", () => {
      const industry =
        (enableCustomIndustry?.checked ? getCustomIndustryFromUI() : getIndustryById(industrySelect.value));
      if (!industry) { clearOutputs(); return; }

      const datumSpec      = buildDatumSpec(industry);
      const paramDatumSpec = buildParamDatumSpec();
      const redeemerSpec   = buildRedeemerSpec(industry);

      const datumOut = { ...datumSpec, paramDatum: paramDatumSpec };
      if (datumJsonOutput)    datumJsonOutput.value    = JSON.stringify(datumOut, null, 2);
      if (redeemerJsonOutput) redeemerJsonOutput.value = JSON.stringify(redeemerSpec, null, 2);

      if (haskellOutput)     haskellOutput.value     = mkHaskellPreview(datumSpec, redeemerSpec, paramDatumSpec);
      if (haskellFullOutput) haskellFullOutput.value = mkFullHaskellModule(datumSpec, redeemerSpec, paramDatumSpec);

      if (plutusTestsOutput) {
        plutusTestsOutput.value = mkValidatorLogicSpecModule(
          datumSpec,
          redeemerSpec,
          paramDatumSpec
        );
      }

      const allConstraintIdsPerAction = buildAllConstraintMapForFormal(industry, redeemerSpec);
      if (formalTestsOutput) {
        formalTestsOutput.value = mkFormalProofModule(
          datumSpec,
          redeemerSpec,
          paramDatumSpec,
          allConstraintIdsPerAction
        );
      }
    });
  }
});

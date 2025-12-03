
  import { templateFile } from './boilerPlate/validators/template.js';
  import { escrowFile } from './boilerPlate/validators/escrow.js';
  import { flashloansFile } from './boilerPlate/validators/flashloans.js';
  import { donationFile } from './boilerPlate/validators/donation.js';
  import { swapFile } from './boilerPlate/validators/swap.js';
  import { willFile } from './boilerPlate/validators/will.js';
  import { votingFile } from './boilerPlate/validators/voting.js';
  import { attendanceFile } from './boilerPlate/validators/attendance.js';

  const coxyPlutusBuilderFile = '../../../coxy-plutus-builder/index.html';
  const coxyPlutusBuilderTutorialFile = '../../../coxy-plutus-builder/coxy-plutus-builder-tutorial.html';

  /* ===== THEME SWITCHING ===== */
  const THEME_KEY = "haskell_plutus_playground_theme";
  const themes = ["midnight","sunrise","forest","terminal","solarized","cardano"];

  function applyTheme(name) {
    const body = document.body;
    themes.forEach(t => body.classList.remove("theme-" + t));
    body.classList.add("theme-" + name);

    document.querySelectorAll(".theme-pill").forEach(btn => {
      btn.classList.toggle("is-active", btn.dataset.theme === name);
    });

    try {
      localStorage.setItem(THEME_KEY, name);
    } catch (e) {}
  }

  document.querySelectorAll(".theme-pill").forEach(btn => {
    btn.addEventListener("click", () => applyTheme(btn.dataset.theme));
  });

  let savedTheme = null;
  try {
    savedTheme = localStorage.getItem(THEME_KEY);
  } catch (e) {}
  if (savedTheme && themes.includes(savedTheme)) {
    applyTheme(savedTheme);
  } else {
    applyTheme("midnight");
  }

  /* ===== Existing logic ===== */

  function generateTemplate() {
    const code = document.getElementById('onchainCode');
    code.value = templateFile;
    logDebug("Generated template Validator module.");
  }

  function loadFiles(template, filename) {
    const code = document.getElementById('onchainCode');
    const tutorialFrame = document.getElementById('tutorialFrame');
    const tutorialPanel = document.getElementById('tutorial');
    const onchainCodeFrame = document.getElementById('onchainCodeTutorialFrame');

    let templateModule = null;

    switch (template) {
      case 'template':
        templateModule = templateFile; break;
      case 'escrow':
        templateModule = escrowFile; break;
      case 'flashloans':
        templateModule = flashloansFile; break;
      case 'donation':
        templateModule = donationFile; break;
      case 'swap':
        templateModule = swapFile; break;
      case 'will':
        templateModule = willFile; break;
      case 'voting':
        templateModule = votingFile; break;
      case 'attendance':
        templateModule = attendanceFile; break;

      // special: Coxy Plutus Builder
      case 'coxyplutusbuilder': {
        onchainCodeFrame.src = coxyPlutusBuilderFile;
        onchainCodeFrame.style.display = 'block';
        code.style.display = 'none';

        tutorialFrame.src = coxyPlutusBuilderTutorialFile;
        tutorialPanel.style.display = 'flex';

        logDebug(
          `✅ Loaded Coxy Plutus Builder (left): ${coxyPlutusBuilderFile} ` +
          `and tutorial (right): ${coxyPlutusBuilderTutorialFile}`
        );
        return;
      }

      default:
        logDebug(`❌ Unknown template: ${template}`);
        return;
    }

    // generic behavior
    code.style.display = 'block';
    onchainCodeFrame.src = '';
    onchainCodeFrame.style.display = 'none';

    code.value = templateModule;
    tutorialFrame.src = filename;
    tutorialPanel.style.display = 'flex';

    logDebug(`✅ Loaded ${template} template and tutorial: ${filename}`);
  }

  function downloadCode() {
    const code = document.getElementById("onchainCode").value || "";
    if (!code.trim()) {
      alert("Code is empty or not found.");
      return;
    }

    const blob = new Blob([code], { type: "text/plain" });
    const url = URL.createObjectURL(blob);

    const a = document.createElement("a");
    a.href = url;
    a.download = "plutus_validator.hs";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);

    URL.revokeObjectURL(url);
  }

  function generateEscrowTemplate() {
    document.getElementById('onchainCode').value = escrowFile;
    logDebug(`Generated Escrow Validator module.`);
  }

  function generateFlashloansTemplate() {
    document.getElementById('onchainCode').value = flashloansFile;
    logDebug("Generated Flashloans Validator module.");
  }

  function generateDonationTemplate() {
    document.getElementById('onchainCode').value = donationFile;
    logDebug("Generated Donation Validator module.");
  }

  function generateSwapTemplate() {
    document.getElementById('onchainCode').value = swapFile;
    logDebug("Generated Swap Validator module.");
  }

  function generateWillTemplate() {
    document.getElementById('onchainCode').value = willFile;
    logDebug("Generated Will Validator module.");
  }

  function generateVotingTemplate() {
    document.getElementById('onchainCode').value = votingFile;
    logDebug("Generated Voting Validator module.");
  }

  function generateAttendanceTemplate() {
    document.getElementById('onchainCode').value = attendanceFile;
    logDebug("Generated Attendance Validator module.");
  }

  function showEditor(id) {
    document.querySelectorAll('.editor').forEach(e => e.classList.remove('active'));
    document.getElementById(id).classList.add('active');

    if (id !== 'onchain') {
      document.getElementById('tutorial').style.display = 'none';
    }

    logDebug(`Switched to ${id} editor`);
  }

  const sidebar = document.getElementById('sidebar');
  document.getElementById('toggleBtn').addEventListener('click', () => {
    sidebar.classList.toggle('collapsed');
    logDebug('Sidebar toggled');
  });

  const debugConsole = document.getElementById('debugConsole');

  function logDebug(msg) {
    const now = new Date().toLocaleTimeString();
    debugConsole.textContent += `\n[${now}] ${msg}`;
    debugConsole.scrollTop = debugConsole.scrollHeight;
  }

  function clearLogs() {
    debugConsole.textContent = "[INFO] Logs cleared.";
  }

  function saveCode() {
    logDebug("Code saved (simulated).");
  }

  async function copyCode() {
    const code = document.getElementById('onchainCode').value;
    if (!code.trim()) {
      logDebug("No code to copy!");
      return;
    }
    try {
      await navigator.clipboard.writeText(code);
      logDebug("✅ Code copied to clipboard!");
    } catch (err) {
      logDebug("❌ Failed to copy code: " + err);
    }
  }

  function hardRefresh() {
    const url = new URL(window.location.href);
    url.searchParams.set('_ts', Date.now().toString());
    window.location.replace(url.toString());
  }

  Object.assign(window, {
    hardRefresh,
    copyCode,
    showEditor,
    logDebug,
    clearLogs,
    saveCode,
    downloadCode
  });

  document.addEventListener("DOMContentLoaded", () => {
    const templateButtons = document.querySelectorAll('[data-template][data-filename]');
    templateButtons.forEach(btn => {
      btn.addEventListener('click', () => {
        const template = btn.getAttribute('data-template');
        const filename = btn.getAttribute('data-filename');
        loadFiles(template, filename);
      });
    });
  });

  document.addEventListener("DOMContentLoaded", () => {
  const templateButtons = document.querySelectorAll('[data-template][data-filename]');

  templateButtons.forEach(btn => {
    btn.addEventListener('click', () => {
      const template = btn.getAttribute('data-template');
      const filename = btn.getAttribute('data-filename');

      // existing behavior
      loadFiles(template, filename);

      // highlight active pill
      templateButtons.forEach(b => b.classList.remove('active-template'));
      btn.classList.add('active-template');
    });
  });
});

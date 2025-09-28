
# 🚀 Nix Setup Tutorial (with Problems & Solutions)

## 📑 Table of Contents

1. 🔎 Introduction
2. ⚖️ Choosing an Installation Mode
   2.1 🖥️ Multi-user (daemon)
   2.2 👤 Single-user (no daemon)
3. 🧹 Preparing a Clean Environment
4. 📥 Installing Nix
   4.1 🖥️ Multi-user install
   4.2 👤 Single-user install
5. ❗ Common Problems and Solutions
   5.1 🔌 Daemon socket connection refused
   5.2 📂 Backup file conflicts (`*.backup-before-nix`)
   5.3 📦 `git+file://` input errors
   5.4 🔑 Store permissions errors
6. 🩺 Post-install Sanity Checks
7. 🧩 Configuring Flakes
8. 📖 Glossary of Terms

---

## 1. 🔎 Introduction

Nix is a powerful package manager and build system. It can be installed in **multi-user (daemon)** mode (recommended for desktops/servers) or **single-user (no daemon)** mode (simpler, good for WSL or laptops without systemd). Picking one mode and sticking with it avoids most headaches.

---

## 2. ⚖️ Choosing an Installation Mode

### 2.1 🖥️ Multi-user (daemon)

* ✅ Best for servers and desktops with multiple users.
* 🛠 Requires `nix-daemon` systemd service.
* 👥 Your account must be in the `nix-users` group.

### 2.2 👤 Single-user (no daemon)

* ✅ Best for WSL or laptops without systemd.
* 🧍 No background service, simpler to maintain.
* 🔐 Your user owns `/nix` directly.

---

## 3. 🧹 Preparing a Clean Environment

If you’ve reinstalled Nix multiple times, leftovers may cause conflicts. Clean them up:

```bash
sudo systemctl stop nix-daemon 2>/dev/null || true
sudo rm -f /etc/nix/nix.conf
rm -rf ~/.config/nix
```

👉 Also save and clear old `*.backup-before-nix` files (see §5.2).

---

## 4. 📥 Installing Nix

### 4.1 🖥️ Multi-user install

```bash
sh <(curl -L https://nixos.org/nix/install) --daemon
```

Then:

```bash
sudo systemctl enable --now nix-daemon
sudo usermod -aG nix-users "$USER"
newgrp nix-users
```

### 4.2 👤 Single-user install

```bash
sh <(curl -L https://nixos.org/nix/install) --no-daemon
echo 'export NIX_REMOTE=local' >> ~/.bashrc
. ~/.bashrc
```

---

## 5. ❗ Common Problems and Solutions

### 5.1 🔌 Daemon socket connection refused

**Error:**

```
error: cannot connect to socket at '/nix/var/nix/daemon-socket/socket': Connection refused
```

**Cause:** Daemon expected, but not running.
**Fix:**

* 🖥️ If using daemon → install with `--daemon` and enable `nix-daemon`.
* 👤 If using single-user → `unset NIX_REMOTE && export NIX_REMOTE=local`.

---

### 5.2 📂 Backup file conflicts

**Error:**

```
I need to back up /etc/bashrc to /etc/bashrc.backup-before-nix,
but the latter already exists.
```

**Cause:** Old installer backup still exists.
**Fix:**

```bash
sudo cp /etc/bashrc /root/bashrc.safety-$(date +%F)
sudo mv /etc/bashrc.backup-before-nix /etc/bashrc
```

---

### 5.3 📦 `git+file://` input errors

**Error:**

```
… while fetching the input 'git+file:///home/user/project'
```

**Cause:** Directory isn’t a Git repo.
**Fix:**

```bash
cd ~/project
git init
git add -A
git commit -m "init"
```

👉 Or replace with a `path:` input in `flake.nix`.

---

### 5.4 🔑 Store permissions errors

**Error:** Permission denied in `/nix/store`.
**Cause:** Leftover daemon install permissions.
**Fix:**

```bash
sudo chown -R "$USER":"$USER" /nix
```

---

## 6. 🩺 Post-install Sanity Checks

```bash
nix doctor
nix show-config | grep experimental-features
```

Expected ✅:

* `[PASS] Client protocol matches store protocol`
* `You are trusted by store uri: local` (if single-user)

---

## 7. 🧩 Configuring Flakes

Enable flakes:

```bash
mkdir -p ~/.config/nix
echo 'experimental-features = nix-command flakes' >> ~/.config/nix/nix.conf
```

Check:

```bash
nix flake show
```

---

## 8. 📖 Glossary of Terms

* **Nix** 🧰 → Package manager and build system.
* **Daemon** ⚙️ → Background service managing builds.
* **Single-user** 👤 → One user owns `/nix`, no daemon.
* **Flakes** 🧩 → Modern Nix feature for reproducibility.
* **gcroot** 🗄️ → Prevents garbage collection of packages.
* **`git+file://` input** 🔗 → Flake input syntax that requires a Git repo.
* **`NIX_REMOTE`** 🔌 → Env var telling Nix whether to use `local` or `daemon`.
* **Profiles** 📦 → Collections of installed packages per user.

---

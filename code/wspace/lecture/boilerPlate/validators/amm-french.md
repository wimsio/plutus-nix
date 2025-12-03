Constant Product Automated Market Maker (AMM) DEX — A Complete Tutorial (Cardano/Plutus)
Auteur : Coxygen Global Date & Heure : Mercredi 12 novembre 2025 à 07:50 (Africa/Johannesburg) Licence MIT

Table of Contents
🔰 Introduction
🧩 Concepts et définitions de base
🏗️ Architecture du système (UTxO Cardano)
🔄 Parcours utilisateurs principaux
🧮 Mathématiques & exemples détaillés
🧑‍⚖️ Règles du validateur (Haskell/Plutus V2)
🛡️ Exigences de sécurité & modèle de menace
🚀 Monter en charge jusqu’à un concurrent de classe Uniswap
🧰 Pratiques opérationnelles & outillage
📚 Glossaire des termes
📎 Annexe : Q&R précédentes (« Tu as dit… »)
🧩 Implémentation de référence (Compilation de AMM.hs)

1. 🔰 Introduction
   Les Automated Market Makers (AMM) remplacent les carnets d’ordres par des pools de liquidité on-chain gouvernés par une règle de tarification simple. Ce tutoriel vous guide dans la conception et la construction d’un AMM à produit constant (CPMM) pour un DEX sur Cardano en utilisant Plutus V2, depuis les bases mathématiques jusqu’aux règles du validateur, en passant par la sécurité de niveau production et une feuille de route pour évoluer vers un échange de classe Uniswap.

Ce guide est entièrement synchronisé avec un fichier AMM.hs compilable qui :

* Implémente un validateur de pool (AddLiquidity, Swap, RemoveLiquidity)
* Implémente une politique de mint/burn des LP, protégée par le NFT du pool + la participation
* Utilise des frais configurables en “basis points” (Bps), des garde-fous de slippage et plusieurs LP
* Exporte des fichiers .plutus (enveloppes texte cardano-cli) pour les deux scripts

2. 🧩 Concepts et définitions de base
   **LP (Liquidity Provider)** — Dépose les deux actifs dans un pool et reçoit des tokens LP représentant une propriété fractionnaire des réserves et des frais.

**CPMM (Constant-Product Market Maker)** — Maintient ((x\cdot y \approx k)), les frais faisant croître (k). Le prix spot ≈ ((y/x)) pour X en unités de Y.

**TVL (Total Value Locked)** — Valeur totale de tous les actifs verrouillés dans un protocole/pool. TVL plus élevé ⇒ liquidité plus profonde ⇒ slippage plus faible.

**AMM (Automated Market Maker)** — Exchange par smart contract qui cote les prix de façon algorithmique (par ex. CPMM) au lieu de faire du matching d’ordres.

**Slippage (glissement de prix)** — Différence entre la sortie attendue et l’exécution réelle, due à l’impact sur le prix et à la latence/MEV. Contrôlé avec minOut/maxIn.

3. 🏗️ Architecture du système (Cardano UTxO)

3.1 Identité du pool
Le NFT du pool verrouillé dans l’UTxO du pool identifie de manière unique le pool à travers les transitions.

Le Datum du pool stocke les réserves et (optionnellement) des indices de supply mis en cache. Dans cette construction didactique :

* Réserves : dAdaR, dTokR
* Les IDs d’actifs sont immuables dans PoolParams : token (CS,TN), LP (CS,TN), NFT du pool (CS,TN)
* Taux de frais `ppFeeBps :: Bps` (par ex. `Bps 100 = 1%`)

3.2 Forme des transactions

* Add Liquidity : 1 entrée du pool → 1 sortie continuante (NFT préservé), mint de LP vers le déposant
* Swap : 1 entrée du pool → 1 sortie continuante, aucun mint/burn de LP
* Remove Liquidity : 1 entrée du pool → 1 sortie continuante, burn de LP depuis le retraitant

3.3 Reference Scripts & Inline Datums

* Préférer les *reference scripts* pour l’efficacité en frais.
* Supporter à la fois `OutputDatum` et `OutputDatumHash` (le validateur gère les deux lors du décodage des datums).

4. 🔄 Parcours utilisateurs principaux

4.1 Création du pool

1. Mintez le NFT du pool ; 2) Créez l’UTxO initial du pool avec le datum `{dAdaR=0, dTokR=0}` et le NFT ; 3) La première liquidité amorce la supply de LP.

4.2 Add Liquidity
Le déposant fournit ADA + Token proportionnellement aux réserves actuelles.
Vérifications : proportionnalité, deltas de réserve exacts, unique triple de mint de LP, continuité du NFT, mise à jour du datum.

4.3 Swap (ADA↔Token)
Le trader soumet l’actif entrant et un `minOut` (tolérance au slippage).
Vérifications : input ajusté des frais, sortie CPMM, transitions de réserves exactes, aucun mint/burn, continuité du NFT.

4.4 Remove Liquidity
Le LP brûle `lpBurn` ; il reçoit une quantité d’ADA et de tokens prorata (le validateur vérifie via les deltas).
Vérifications : triple de burn de LP exact, paiements minimums, les réserves n’augmentent pas, continuité du NFT.

5. 🧮 Mathématiques & exemples détaillés

5.1 Invariant & prix
Sans frais : (((x+\Delta x)(y-\Delta y)=xy\Rightarrow \Delta y=\dfrac{\Delta x,y}{x+\Delta x})). Avec frais ((\gamma)) : ((\Delta x_\text{eff}=\Delta x(1-\gamma))) — utiliser ((\Delta x_\text{eff})) dans la formule.

5.2 Mint de LP (pool existant)
Étant donné des réserves ((x,y)), un total de LP (L), un dépôt ((\Delta x,\Delta y)) proportionnel :
[
\text{LP}*\text{minted} = L\cdot\min\bigl(\tfrac{\Delta x}{x},\tfrac{\Delta y}{y}\bigr)
]
Si parfaitement proportionnel, (\text{LP}*\text{minted} = L*(\Delta x/x)).

Exemple : si x reçoit 10 et que vous ajoutez (\Delta y = y*(10/x)) correspondante, alors (\text{LP}_\text{minted} = L*(10/x)).

5.3 LP de bootstrap (pool vide)
Règle courante : ((\text{LP}_\text{minted} = \lfloor\sqrt{\Delta x\cdot\Delta y}\rfloor)) (optionnellement mise à l’échelle), moins un petit minimum verrouillé.

5.4 Slippage
(\text{slippage%} = (expected − actual)/expected × 100%). Protéger avec `minOut` ou `maxIn`.

6. 🧑‍⚖️ Règles du validateur (Haskell/Plutus V2)

6.1 Types & instances (prêts pour on-chain)

```haskell
newtype Bps = Bps Integer
```

```haskell
PlutusTx.unstableMakeIsData ''Bps
PlutusTx.makeLift ''Bps
```

**PoolParams** — IDs d’actifs immuables et frais en bps :

```haskell
PlutusTx.unstableMakeIsData ''PoolParams
PlutusTx.makeLift ''PoolParams
```

`PoolDatum { dAdaR, dTokR }` et `Action (AddLiquidity | Swap | RemoveLiquidity)` dérivent tous deux `IsData`.

L’égalité de tuples pour les vérifications `flattenValue` utilise `{-# LANGUAGE FlexibleInstances #-}` et une petite instance `Eq (CurrencySymbol, TokenName, Integer)`.

6.2 Primitifs d’aide
`adaOf`, `tokOf`, `lpOf`, `nftOf` pour l’extraction des actifs ; `findOwnInput'` (strict) et `decodeDatum` pour charger les datums.

Frais & CPMM :

```haskell
feeEff (Bps bps) x = divide (x * (10000 - bps)) 10000

cpmmOut x y dxEff =
  if dxEff <= 0 then 0 else divide (dxEff * y) (x + dxEff)
```

`ensure :: Bool -> ()` comme helper pratique de vérification.

6.3 Validateur du pool

```haskell
mkPoolValidator :: PoolParams -> PoolDatum -> Action -> ScriptContext -> Bool
```

Garde-fous communs :

1. Exactement une sortie continuante ;
2. Le NFT du pool est présent à l’entrée et à la sortie ;
3. Décodage des datums avant/après ;
4. (Optionnel) Interdire les actifs étrangers via un scan `flattenValue`.

**AddLiquidity dAda dTok minLP**

* Bootstrap autorisé lorsque `adaR == 0 && tokR == 0` avec `lpMinted = ⌊√(dAda*dTok)⌋`.
* Sinon, imposer la proportionnalité : `dAda*tokR == dTok*adaR`.
* Mise à jour exacte des réserves ; unique triple de mint de LP : `flattenValue minted == [(ppLpCS, ppLpTN, lpMinted)]` et `lpOf pp minted == lpMinted`.

**Swap dir amount minOut**

* `dir=True` ADA→Token ; `False` Token→ADA.
* Appliquer `feeEff` à l’input puis CPMM ; imposer la mise à jour des réserves et `dy ≥ minOut` ; aucun mint/burn.

**RemoveLiquidity lpBurn minAdaOut minTokOut**

* Exiger un triple de burn de LP exact : `flattenValue minted == [(ppLpCS, ppLpTN, negate lpBurn)]`.
* Calculer les deltas : `dAdaOut = adaR - adaR'`, `dTokOut = tokR - tokR'` ; imposer `dAdaOut ≥ minAdaOut && dTokOut ≥ minTokOut` et des réserves non croissantes.

6.4 Politique de mint des LP (sans dépendances externes)

```haskell
mkLpPolicy :: PoolParams -> () -> ScriptContext -> Bool
```

Valide si et seulement si la transaction dépense une entrée de pool portant le NFT du pool.

Wrapper non typé utilisant un décodage manuel (aucun besoin de `plutus-script-utils`) :

```haskell
wrappedMkLpPolicy :: PoolParams -> BuiltinData -> BuiltinData -> ()
wrappedMkLpPolicy pp d1 d2 =
  let r   = unsafeFromBuiltinData @() d1
      ctx = unsafeFromBuiltinData @ScriptContext d2
  in if mkLpPolicy pp r ctx then () else traceError "mkLpPolicy failed"
```

Compilé avec Template Haskell et `liftCode pp` dans une `MintingPolicy`.

6.5 Arrondis entiers & sécurité

* Le chemin de mint arrondit vers le bas ;
* Le swap utilise une division entière sûre ;
* Le chemin de burn vérifie les deltas exacts et une quantité de mint négative.

7. 🛡️ Exigences de sécurité & modèle de menace

7.1 Identité & état
Le NFT du pool lie l’état ; vérifier précisément les IDs des tokens (CS,TN) et ADA (`adaSymbol`, `adaToken`).
Tout l’état provient du datum ; le redeemer porte uniquement l’action/les minimums/les paramètres.

7.2 Discipline de mint/burn

* Add → mint uniquement ;
* Remove → burn uniquement ;
* Swap → aucun mint/burn.
  Interdire les autres actifs dans `txInfoMint`.

7.3 Comptabilité de la valeur
Imposer des transitions de réserves exactes ; autoriser uniquement la “min-ADA dust” ; sinon interdire les actifs étrangers dans l’UTxO du pool.

7.4 Slippage & MEV
`minOut` / `maxIn` obligatoires. Optionnel : enchères par lots, quotes limitées dans le temps, mempools privés.

7.5 Contrôles d’accès (optionnels)
Clé de gouvernance pour basculer/ajuster les frais et paramètres ; *circuit breaker* pour des deltas anormaux.

7.6 Audits & tests

* Tests de propriétés : les frais rendent (k) non décroissant ; aucun mint involontaire ; continuité du NFT.
* Fuzz des cas de bord d’arrondi ; tests de mutation ; analyse statique ; revues manuelles.

8. 🚀 Monter en charge jusqu’à un concurrent de classe Uniswap

* Paliers de frais (0,05 %, 0,30 %, 1,00 %).
* Courbes de type *stableswap* pour actifs corrélés.
* Liquidité concentrée (ordres en plage ; positions sous forme de NFTs).
* Routage : routes multi-hop et fractionnées.
* Oracles & analytics : flux de prix TWAP ; tableaux de bord pour TVL, volume, frais, IL.

9. 🧰 Pratiques opérationnelles & outillage

9.1 Build

```bash
nix-shell
cabal build
```

9.2 Export des scripts .plutus (déjà dans AMM.hs)
Les sérialiseurs pour `Validator` et `MintingPolicy` produisent des enveloppes texte `cardano-cli` :

```haskell
writeValidatorEnvelope :: FilePath -> Validator     -> IO ()
writePolicyEnvelope    :: FilePath -> MintingPolicy -> IO ()
```

Point d’entrée pratique :

```haskell
exportPlutusScripts :: IO ()
exportPlutusScripts = do
  let poolValidator = poolValidatorScript params
      lpPolicy      = lpMintingPolicy params
  writeValidatorEnvelope "./assets/pool-validator.plutus" poolValidator
  writePolicyEnvelope    "./assets/lp-policy.plutus"      lpPolicy
```

Sorties :

* `./assets/pool-validator.plutus` (validateur du pool)
* `./assets/lp-policy.plutus` (politique de mint/burn des LP)

9.3 Exemple d’utilisation CLI

```bash
cardano-cli transaction build \
  --tx-in ... \
  --tx-out ... \
  --mint-script-file ./assets/lp-policy.plutus \
  --out-file tx.raw
```

(Ajuster les flags selon le réseau et l’usage des scripts.)

9.4 Seeds pour émulateur
IDs de démo : `fakeCS1`, `fakeCS2`, `fakeCS3` ; tokens : `tokenTN`, `lpTN`, `nftTN` et paramètres avec `Bps 100` (1 %).

10. 📚 Glossaire des termes

* **AMM** : Automated Market Maker (teneur de marché automatisé)
* **CPMM** : Constant-Product MM ((x\cdot y = k))
* **LP** : Liquidity Provider ; aussi le token représentant les parts du pool
* **LP Tokens** : Représentation fongible de la propriété du pool
* **TVL** : Total Value Locked
* **Slippage** : Écart entre le prix/sortie attendu(e) et l’exécution réelle
* **MinOut/MaxIn** : Garde-fous de protection contre le slippage
* **Pool NFT** : Token unique liant l’identité de l’UTxO du pool à travers les transitions
* **Datum** : Charge utile de données on-chain attachée à un UTxO
* **Reference Script** : Script stocké on-chain, référencé par de futures transactions
* **TWAP** : Time-Weighted Average Price (prix moyen pondéré dans le temps)
* **CIP-68** : Standard pour les NFTs avec métadonnées/état on-chain

11. 📎 Annexe : Q&R précédentes (« Tu as dit… »)

* **Qu’est-ce qu’un LP ? CPMM ? TVL ? AMM ?** — Voir §2.
* **Qu’est-ce que (x · y = k) ?** — Voir §5.1.
* **Qu’est-ce que le slippage ?** — Voir §2 et §5.4.
* **Si x reçoit 10, combien de tokens LP ?** — Voir §5.2.
* **Comment décide-t-on du total de LP ?** — Supply élastique & politique dans §5.3 et §6.5.

12. 🧩 Implémentation de référence (Compilation de AMM.hs)

Module : `AMM` Exports principaux :

```haskell
poolValidatorScript :: PoolParams -> Validator
lpMintingPolicy    :: PoolParams -> MintingPolicy
exportPlutusScripts :: IO ()  -- écrit les deux enveloppes .plutus
```

Template de paramètres :

```haskell
params :: PoolParams
params = PoolParams
  { ppTokenCS   = "ff01"
  , ppTokenTN   = "TOK"
  , ppLpCS      = "ff02"
  , ppLpTN      = "LP"
  , ppPoolNftCS = "ff03"
  , ppPoolNftTN = "POOLNFT"
  , ppFeeBps    = Bps 100  -- 1%
  }
```


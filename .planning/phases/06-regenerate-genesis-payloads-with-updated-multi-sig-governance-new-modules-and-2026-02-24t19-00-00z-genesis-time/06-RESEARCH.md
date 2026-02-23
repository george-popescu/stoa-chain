# Phase 6: Regenerate Genesis Payloads with Updated Multi-Sig Governance, New Modules, and 2026-02-24T19:00:00Z Genesis Time — Research

**Researched:** 2026-02-23
**Domain:** Chainweb genesis payload generation (Haskell Ea tool, Pact YAML/pact files, version configuration)
**Confidence:** HIGH — all findings verified by direct source code inspection

---

## Summary

Phase 6 replaces the existing 4-file Stoa genesis with a 5-transaction architecture provided by Mihai in `/Users/codera/Development/Stoa/genesis_stoa_update/`. The new genesis deploys: (1) a custom `ns` module with registry, namespaces (`stoa-ns`, `ouronet-ns`, `user`, `free`, `util`) and root-namespace keysets; (2) the coin interfaces (`fungible-v1`, `fungible-xchain-v1`, `gas-payer-v1`, `stoic-fungible-v1`), `stoic-predicates` module, and 7 `stoa_master` keysets with new multi-sig `keys-any`/`all-but-one` predicates; (3) `util.guards` and `util.gas-guards` utility modules; (4) the coin module (updated GOVERNANCE, interfaces now in `stoa-ns` namespace, updated GENESIS-TIME) plus chain-0-only initialisation; (5) the `stoic-xchain` module plus `kadena-xchain-gas` and `stoa-xchain-gas` account creation on all chains.

The Ea tool processes YAML files via `Pact.ApiReq.mkApiReq`. Each of Mihai's `.pact` files needs a thin YAML wrapper. The `Genesis` record in `Ea/Genesis.hs` puts all five transactions into `_coinContract` (a list), with all other fields (`_namespaces`, `_keysets`, `_allocations`, `_coinbase`) set to `Nothing`. The chain-0 vs chains-1-9 difference is handled by two separate YAML files for transaction 4: one that includes `A_InitialiseStoaChain` and one that does not (requiring either a trimmed copy of the pact file or a no-init YAML).

Two time constants must be updated in lockstep: `GENESIS-TIME` inside `stoa-genesis-4.pact` (currently `2026-02-18T21:30:00Z`, target `2026-02-24T19:00:00Z`) and `_genesisTime` in `src/Chainweb/Version/Stoa.hs` (currently `2026-02-01T00:00:00.000000`, target `2026-02-24T19:00:00.000000`). The miner rewards CSV and its SHA512 hash constants are time-independent and do NOT need regeneration.

**Primary recommendation:** Copy all 5 Mihai pact files into `pact/genesis/stoa/`, create YAML wrappers, update both time constants, rewrite the `stoa0`/`stoaN` Genesis records to use 5-element `_coinContract` lists, run the Ea tool (`cabal run ea`), rebuild `chainweb-node`, and redeploy the binary to the server.

---

## Existing Architecture (What Gets Replaced)

### Current Stoa Genesis — 4 Transactions

| Order | YAML File | Content | Chains |
|-------|-----------|---------|--------|
| 1 | `pact/genesis/stoa/load-stoa-coin.yaml` (`_coinContract`) | `new-coin.pact` — 4 interfaces + coin module | 0–9 |
| 2 | `pact/genesis/stoa/ns.yaml` (`_namespaces`) | `ns-install.pact` — ns module + registry | 0–9 |
| 3 | `pact/genesis/stoa/keysets.yaml` (`_keysets`) | 7 `stoa_master` keyset definitions | 0 only |
| 4 | `pact/genesis/stoa/init-chain0.yaml` (`_coinbase`) | `A_InitialiseStoaChain` call | 0 only |

The Ea `Genesis` record for `stoa0` maps these as: `_coinContract=[load-stoa-coin.yaml]`, `_namespaces=Just ns.yaml`, `_keysets=Just keysets.yaml`, `_coinbase=Just init-chain0.yaml`. For `stoaN` (chains 1–9): `_keysets=Nothing`, `_coinbase=Nothing`.

The final tx ordering from `txs = cc <> toList ns <> toList k <> toList a <> toList c` is: coin-contract, ns, keysets, init (chain 0); coin-contract, ns (chains 1–9).

---

## New Architecture — 5 Transactions

### Mihai's 5 Files and What They Deploy

| # | Pact File | JSON Data File | What It Does | Chains |
|---|-----------|----------------|--------------|--------|
| 1 | `stoa-genesis-1.pact` | `stoa-genesis-1.json` | Defines `ns-admin-keyset`, `ns-operate-keyset`, `util-ns-admin`, `util-ns-users` on root namespace; deploys `ns` module; writes registry for `stoa-ns`, `ouronet-ns`, `user`, `free`, `util`; defines those namespaces | 0–9 |
| 2 | `stoa-genesis-2.pact` | `stoa-genesis-2.json` | Enters `stoa-ns` namespace; defines keysets 1–6 (`keys-any`, 2 keys each); deploys `fungible-v1`, `fungible-xchain-v1`, `gas-payer-v1`, `stoic-fungible-v1` interfaces; deploys `stoic-predicates` module; defines keyset 7 with `stoa-ns.stoic-predicates.all-but-one` predicate (4 keys) | 0–9 |
| 3 | `stoa-genesis-3.pact` | none | Enters `util` namespace; deploys `util.guards` and `util.gas-guards` (both `AUTONOMOUS`, non-upgradeable) | 0–9 |
| 4a | `stoa-genesis-4.pact` | `stoa-genesis-4.json` | Deploys coin module (implements `stoa-ns.fungible-v1`, `stoa-ns.fungible-xchain-v1`, `stoa-ns.stoic-fungible-v1`); creates tables; defines foundation keyset; calls `A_InitialiseStoaChain` | **0 only** |
| 4b | `stoa-genesis-4.pact` (trimmed) | `stoa-genesis-4.json` | Same as 4a but WITHOUT the last 3 lines (foundation keyset + `A_InitialiseStoaChain`) | **1–9** |
| 5 | `stoa-genesis-5.pact` | none | Enters `stoa-ns` namespace; deploys `stoic-xchain` module; creates `kadena-xchain-gas` and `stoa-xchain-gas` accounts | 0–9 |

### Key Changes in the New Coin Contract (`stoa-genesis-4.pact` vs `new-coin.pact`)

1. **Interfaces moved to `stoa-ns` namespace**: `implements stoa-ns.fungible-v1`, `stoa-ns.fungible-xchain-v1`, `stoa-ns.stoic-fungible-v1` (not standalone in same file)
2. **GOVERNANCE restructured**: still `enforce-one` of 7 master keysets, same logic
3. **GENESIS-TIME**: `2026-02-18T21:30:00Z` in the file as received — MUST update to `2026-02-24T19:00:00Z`
4. **Foundation init**: Moved to bottom of file (`define-keyset "stoa-foundation-keyset"` + `A_InitialiseStoaChain`) instead of separate yaml
5. **Table schema references updated**: `LocalSupply:{stoa-ns.stoic-fungible-v1.LocalSupplySchema}`

### New Keysets (Significant Security Upgrade)

| Keyset | Keys | Predicate | Notes |
|--------|------|-----------|-------|
| `ns-admin-keyset` (root) | `625ad78...`, `35d7f82...` | `keys-any` | Governs ns registry + stoic-xchain |
| `stoa-ns.stoa_master_one` through `_six` | `625ad78...`, `35d7f82...` | `keys-any` | Coin module governance (any-one-of-7) |
| `stoa-ns.stoa_master_seven` | 4 keys (`625ad78...`, `35d7f82...`, `ed968e0...`, `b28f9a1...`) | `stoa-ns.stoic-predicates.all-but-one` | Special multi-sig |

---

## Ea Tool: How YAML Files Are Processed

### YAML Format (source: `Pact.ApiReq.ApiReq`)

```yaml
# Option A: Reference external pact file
codeFile: relative/path/to/file.pact
data:
  keyset-name: { keys: ["pubkey..."], pred: "keys-any" }
nonce: unique-nonce-string
keyPairs: []

# Option B: Inline code
code: |-
  (pact code here)
data:
  key: value
nonce: unique-nonce-string
keyPairs: []

# Option C: External data file
codeFile: file.pact
dataFile: file.json
nonce: unique-nonce-string
keyPairs: []
```

Supported fields in `ApiReq`: `code`, `codeFile`, `data`, `dataFile`, `nonce`, `keyPairs`. The `codeFile` and `dataFile` paths are **relative to the YAML file's location**.

### Ea Tool Flow (source: `cwtools/ea/Ea.hs`)

```
mkPayloads [stoa0, stoaN]
  → for each Genesis: mkPayload genesis
    → txs = cc <> toList ns <> toList k <> toList a <> toList c
    → for each chain in range: mkChainwebTxs l txs
      → for each YAML file: mkTx yamlFile = snd <$> mkApiReq yamlFile
        → parses YAML, loads codeFile/code, data/dataFile
        → builds Pact4.UnparsedTransaction with chainId set to l (first chain)
      → genPayloadModule version tag chainId txs
        → runs Pact service (execNewGenesisBlock) against fresh RocksDB
        → generates PayloadWithOutputs
    → evaluate (the payloadModules) — verifies all chains produce identical payload
  → writes Haskell module to src/Chainweb/BlockHeader/Genesis/StoaXPayload.hs
```

The tool runs from the `stoa-chain/` working directory. All YAML `codeFile` paths are relative to where the YAML lives (e.g., `pact/genesis/stoa/`).

### Genesis Record Field Mapping

The `Genesis` data type (source: `cwtools/ea/Ea/Genesis.hs`):
```haskell
data Genesis = Genesis
    { _version :: ChainwebVersion
    , _tag :: T.Text
    , _txChainIds :: ChainIdRange     -- which chains
    , _coinbase :: Maybe FilePath     -- tx appended last
    , _keysets :: Maybe FilePath      -- tx appended 3rd
    , _allocations :: Maybe FilePath  -- tx appended 4th
    , _namespaces :: Maybe FilePath   -- tx appended 2nd
    , _coinContract :: [FilePath]     -- tx list prepended first
    }
-- Final tx order: coinContract ++ namespaces ++ keysets ++ allocations ++ coinbase
```

**For the new 5-transaction setup**, use `_coinContract` as a list of all 5 YAML files in order:
```haskell
stoa0 :: Genesis
stoa0 = Genesis
    { _version = Stoa
    , _tag = "Stoa"
    , _txChainIds = onlyChainId 0
    , _coinbase = Nothing
    , _keysets = Nothing
    , _allocations = Nothing
    , _namespaces = Nothing
    , _coinContract =
        [ "pact/genesis/stoa/stoa-genesis-1.yaml"
        , "pact/genesis/stoa/stoa-genesis-2.yaml"
        , "pact/genesis/stoa/stoa-genesis-3.yaml"
        , "pact/genesis/stoa/stoa-genesis-4-chain0.yaml"  -- with A_InitialiseStoaChain
        , "pact/genesis/stoa/stoa-genesis-5.yaml"
        ]
    }

stoaN :: Genesis
stoaN = stoa0
    & txChainIds .~ mkChainIdRange 1 9
    & coinContract .~
        [ "pact/genesis/stoa/stoa-genesis-1.yaml"
        , "pact/genesis/stoa/stoa-genesis-2.yaml"
        , "pact/genesis/stoa/stoa-genesis-3.yaml"
        , "pact/genesis/stoa/stoa-genesis-4-chains1-9.yaml"  -- without init
        , "pact/genesis/stoa/stoa-genesis-5.yaml"
        ]
```

---

## What Files Need to Change

### Step 1: Copy and Adapt Pact Files

Copy all 5 `.pact` files from `/Users/codera/Development/Stoa/genesis_stoa_update/` to `pact/genesis/stoa/`:

```
pact/genesis/stoa/stoa-genesis-1.pact   (copy as-is)
pact/genesis/stoa/stoa-genesis-2.pact   (copy as-is)
pact/genesis/stoa/stoa-genesis-3.pact   (copy as-is)
pact/genesis/stoa/stoa-genesis-4.pact   (copy, then update GENESIS-TIME)
pact/genesis/stoa/stoa-genesis-4-no-init.pact  (copy of stoa-genesis-4.pact, remove last 3 lines)
pact/genesis/stoa/stoa-genesis-5.pact   (copy as-is)
```

Update `GENESIS-TIME` in `stoa-genesis-4.pact`: `"2026-02-18T21:30:00Z"` → `"2026-02-24T19:00:00Z"`.

### Step 2: Create YAML Wrappers

Create 6 YAML files in `pact/genesis/stoa/`:

**stoa-genesis-1.yaml** (uses inline data from stoa-genesis-1.json):
```yaml
codeFile: stoa-genesis-1.pact
data:
  payload_ns-admin-keyset:
    keys: ["625ad78ab8c1df826d69e1f0e6457334b8e085b7d256d10be41726ced17fdf74", "35d7f82a7754d10fc1128d199aadb51cb1461f0eb52f4fa89790a44434f12ed8"]
    pred: "keys-any"
  payload_ns-genesis-keyset:
    keys: ["1a4e15d3c51e0b73e92644600487ba8eaae312e1a178b91801d54e13c1b350a5", "10b998049806491ec3e26f7507020554441e6b9271cfee1779d85230139c92df"]
    pred: "keys-any"
  payload_util-ns-admin:
    keys: ["10e05871c5a2ef3f0f7dca9edd8e96e4ef0952175a4a2621895d2c4402a7b56a", "725d6ad18c7e4ef6e2773f6bb315bde13437872d0235f6404c0c99d9d900bbb4"]
    pred: "keys-all"
  payload_util-ns-users:
    keys: ["b836c5c6b989d97737c954934c24686b876b78082bd03475878c245794d6ef80", "5047d039d1d918e3489f42a52a46b54cb5b3b259e42dd2e43c071fe2b77863f2"]
    pred: "keys-all"
nonce: stoa-genesis-1
keyPairs: []
```

**stoa-genesis-2.yaml** (uses inline data from stoa-genesis-2.json):
```yaml
codeFile: stoa-genesis-2.pact
data:
  payload_stoa-master-one:
    keys: ["625ad78ab8c1df826d69e1f0e6457334b8e085b7d256d10be41726ced17fdf74", "35d7f82a7754d10fc1128d199aadb51cb1461f0eb52f4fa89790a44434f12ed8"]
    pred: "keys-any"
  # ... (master-two through master-six same keys, keys-any)
  payload_stoa-master-seven:
    keys: ["625ad78...", "35d7f82...", "ed968e0b...", "b28f9a1a..."]
    pred: "stoa-ns.stoic-predicates.all-but-one"
nonce: stoa-genesis-2
keyPairs: []
```

**stoa-genesis-3.yaml** (no data):
```yaml
codeFile: stoa-genesis-3.pact
nonce: stoa-genesis-3
keyPairs: []
```

**stoa-genesis-4-chain0.yaml** (with foundation keyset data, points to full file):
```yaml
codeFile: stoa-genesis-4.pact
data:
  payload_stoa-foundation:
    keys: ["625ad78ab8c1df826d69e1f0e6457334b8e085b7d256d10be41726ced17fdf74", "35d7f82a7754d10fc1128d199aadb51cb1461f0eb52f4fa89790a44434f12ed8"]
    pred: "keys-any"
nonce: stoa-genesis-4-chain0
keyPairs: []
```

**stoa-genesis-4-chains1-9.yaml** (no data, points to trimmed file):
```yaml
codeFile: stoa-genesis-4-no-init.pact
nonce: stoa-genesis-4-chains1-9
keyPairs: []
```

**stoa-genesis-5.yaml** (no data):
```yaml
codeFile: stoa-genesis-5.pact
nonce: stoa-genesis-5
keyPairs: []
```

### Step 3: Update GENESIS-TIME in `stoa-genesis-4.pact`

```pact
;; Line 84 in genesis_stoa_update/stoa-genesis-4.pact
(defconst GENESIS-TIME (time "2026-02-18T21:30:00Z"))
;; Change to:
(defconst GENESIS-TIME (time "2026-02-24T19:00:00Z"))
```

### Step 4: Update `src/Chainweb/Version/Stoa.hs`

```haskell
-- Current:
, _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2026-02-01T00:00:00.000000 |]
-- Change to:
, _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2026-02-24T19:00:00.000000 |]
```

### Step 5: Update `cwtools/ea/Ea/Genesis.hs`

Rewrite `stoa0` and `stoaN` records as described in the field mapping section above. Also update `stoaCoinContract`, `stoaNs`, `stoaKeysets`, `stoaInitChain0` constants — or simply remove them if no longer referenced.

### Step 6: Update `pact/stoa-coin/new-coin.pact`

This file is the existing coin contract used for REPL testing. Update it to match `stoa-genesis-4.pact` (or replace it entirely). This is needed because the contract is also referenced for coinbase at runtime.

**CRITICAL**: The `pact/stoa-coin/new-coin.pact` file is what the live node uses for coinbase execution. It must be replaced with the new contract that references `stoa-ns.fungible-v1` (namespaced interfaces). The GENESIS-TIME must match.

### Step 7: Run Ea Tool

```bash
cd /Users/codera/Development/Stoa/stoa-chain
cabal run ea
```

This regenerates:
- `src/Chainweb/BlockHeader/Genesis/Stoa0Payload.hs`
- `src/Chainweb/BlockHeader/Genesis/Stoa1to9Payload.hs`

### Step 8: Rebuild and Redeploy

```bash
cabal build chainweb-node
# Deploy new binary to node1.stoachain.com before genesis time
```

---

## Critical Facts and Constraints

### GENESIS-TIME Must Be Consistent

The `GENESIS-TIME` constant in `stoa-genesis-4.pact` (the deployed coin contract) and `_genesisTime` in `Stoa.hs` MUST be identical. The contract uses `GENESIS-TIME` for:
1. Gas price progression (`UC_MinimumGasPriceANU`): starts at 10,000 ANU, grows 1 ANU per 3 hours
2. Year-0 block count (`UC_YearZeroBlocks`): affects year-0 emission
3. Stoa-year calculation (`URC_GetStoaYear`): which year's emission formula to use

Mismatch between `_genesisTime` in Stoa.hs and `GENESIS-TIME` in the contract would cause incorrect emission calculations.

### Miner Rewards CSV — NO Change Needed

The CSV at `rewards/miner_rewards.csv` and its SHA512 hashes in `MinerReward.hs` are **time-independent block-index schedules**. The genesis time change does NOT require CSV regeneration. The two hash constants (`expectedMinerRewardsHash` and `expectedRawMinerRewardsHash`) remain valid.

### stoa-genesis-4.pact: Chain 0 vs Chains 1–9 Split

The file ends with:
```pact
(create-table coin-table)           ;; line 1650
(create-table LocalSupply)          ;; line 1651
(create-table UR|StoaTable)         ;; line 1653
(create-table UR|LocalSupply)       ;; line 1654
(create-table URV|UrStoaVault)      ;; line 1656
(create-table URV|UrStoaVaultUser)  ;; line 1657

;;2]Initialises StoaChain on Chain 0
(define-keyset "stoa-foundation-keyset" (read-keyset "payload_stoa-foundation"))  ;; line 1660
(A_InitialiseStoaChain "stoa-foundation" (keyset-ref-guard "stoa-foundation-keyset"))  ;; line 1661
```

`stoa-genesis-4-no-init.pact` = lines 1–1657 only (coin module + create-table, no init call). The JSON data file (`stoa-genesis-4.json`) with `payload_stoa-foundation` is only needed for the chain-0 YAML wrapper; the chains-1-9 wrapper can reference the no-init pact with no data (or empty data).

**Note**: `A_InitialiseStoaChain` itself calls `UEV_ChainZero` (enforces chain-id == "0"), so running the full file on chains 1–9 would fail at that enforce. The split is mandatory.

### Governance in Genesis Transactions (Confidence: MEDIUM)

The previous Phase 3 genesis ran `A_InitialiseStoaChain` (which requires GOVERNANCE) with `keyPairs: []` and succeeded. The GOVERNANCE cap in the coin module uses `enforce-one` of 7 keyset guards. In Pact 4, genesis block transactions run through `execTransactions` — the governance check is NOT bypassed by a special "genesis mode". The Phase 3 success implies one of:

- Pact allows module-level governance caps to be acquired without signatures during first deployment context
- The `enforce-one` with all failing branches causes a specific error that is treated as success in genesis

**Pragmatic approach**: follow the same `keyPairs: []` pattern that worked in Phase 3. If stoa-genesis-5.pact's `create-xchain-gas-account` (which requires `(with-capability (GOVERNANCE))`) fails during ea, investigate and add appropriate signing context.

### Transaction Ordering Dependencies

```
tx1 (ns)        → defines ns-admin-keyset, util-ns-users/admin, ns module, namespaces
tx2 (coin-if)   → requires stoa-ns namespace (from tx1), defines interfaces + predicates + keysets 1-7
tx3 (utils)     → requires util namespace (from tx1), defines util.guards + util.gas-guards
tx4 (coin)      → implements stoa-ns.fungible-v1 etc. (from tx2), reads foundation keyset (chain 0 only)
tx5 (xchain)    → uses coin (tx4), util.guards (tx3), util.gas-guards (tx3), enforces ns-admin-keyset (tx1)
```

This ordering is correct and must be preserved in the `_coinContract` list.

---

## Common Pitfalls

### Pitfall 1: YAML codeFile Path Resolution

**What goes wrong**: The `codeFile` path in a YAML is relative to the YAML file's location. If the YAML is at `pact/genesis/stoa/stoa-genesis-1.yaml` and references `codeFile: stoa-genesis-1.pact`, the pact file must be at `pact/genesis/stoa/stoa-genesis-1.pact`.

**Prevention**: Keep all .pact and .yaml files in the same directory (`pact/genesis/stoa/`). Use bare filenames (no path components) for `codeFile`.

### Pitfall 2: Data Key Format in YAML

**What goes wrong**: Pact keyset keys in the YAML `data` section must use the `{"keys": [...], "pred": "..."}` object form, not a bare array, when the predicate is not `"keys-all"`. The old keysets used bare arrays: `stoa-ns.stoa_master_one: ["key..."]` which defaults to `keys-all`.

**New keysets use multi-key/non-default-pred form**:
```yaml
data:
  payload_ns-admin-keyset:
    keys: ["key1", "key2"]
    pred: "keys-any"
```

**Prevention**: Use YAML object format with explicit `keys` and `pred` fields for all new keysets.

### Pitfall 3: `stoic-predicates.all-but-one` Availability for `stoa_master_seven`

**What goes wrong**: `stoa-genesis-2.pact` deploys `stoic-predicates` module (line 242) and then uses `stoa-ns.stoic-predicates.all-but-one` as the predicate for `stoa_master_seven` (line 323). This works because the define-keyset call happens AFTER the module is deployed in the same transaction.

However, the YAML `data` section specifies `pred: "stoa-ns.stoic-predicates.all-but-one"` — this is a **user-defined predicate**. Pact must recognize this predicate at keyset definition time. Because the module is deployed earlier in the same tx, this should work.

**Prevention**: Verify this during ea execution. If the ea tool fails on tx2, the issue may be predicate resolution timing.

### Pitfall 4: Genesis Time Must Be in the Past When Node Starts

**What goes wrong**: The node validates blocks against genesis time. If `_genesisTime` in Stoa.hs is set to a future time, the node won't start producing blocks until that time.

**This is intentional**: Set to `2026-02-24T19:00:00Z` (exactly 21:00 UTC+2). The node must be deployed and running BEFORE this time to begin mining at genesis.

**Also**: The ea tool itself runs against a fresh Pact environment. If the `GENESIS-TIME` constant is in the past at ea-tool-run time, year calculations proceed from year 0 correctly.

### Pitfall 5: `pact/stoa-coin/new-coin.pact` Must Be Updated

**What goes wrong**: The runtime Pact service (coinbase execution) uses `pact/stoa-coin/new-coin.pact` as the loadable path for the coin contract. If this file still references standalone interfaces (old format) while the genesis payload is the new format (namespaced interfaces), there's a mismatch at runtime.

**Prevention**: Replace `pact/stoa-coin/new-coin.pact` with the content of `stoa-genesis-4.pact` (with updated GENESIS-TIME). This is a critical file.

**Confidence: MEDIUM** — need to verify during Phase 6 execution that the node runtime actually loads this path for coinbase. The Haskell Pact service may or may not re-load the contract from disk at runtime vs using the compiled genesis payload.

---

## Architecture Patterns

### File Organization After Phase 6

```
pact/genesis/stoa/
├── stoa-genesis-1.pact         (new: ns module + registry)
├── stoa-genesis-1.yaml         (new: wrapper for tx1)
├── stoa-genesis-2.pact         (new: interfaces + stoic-predicates + keysets 1-7)
├── stoa-genesis-2.yaml         (new: wrapper for tx2)
├── stoa-genesis-3.pact         (new: util.guards + util.gas-guards)
├── stoa-genesis-3.yaml         (new: wrapper for tx3)
├── stoa-genesis-4.pact         (new: coin module + init - chain 0)
├── stoa-genesis-4-no-init.pact (new: coin module only - chains 1-9)
├── stoa-genesis-4-chain0.yaml  (new: wrapper for tx4 chain 0)
├── stoa-genesis-4-chains1-9.yaml (new: wrapper for tx4 chains 1-9)
├── stoa-genesis-5.pact         (new: stoic-xchain + gas accounts)
├── stoa-genesis-5.yaml         (new: wrapper for tx5)
│
│   [OLD FILES - kept for reference or deleted]
├── ns.yaml                     (old: replaced by stoa-genesis-1.yaml)
├── keysets.yaml                (old: replaced by stoa-genesis-2.yaml)
├── load-stoa-coin.yaml         (old: replaced by stoa-genesis-4-*.yaml)
└── init-chain0.yaml            (old: now embedded in stoa-genesis-4.pact)
```

### Ea Genesis Record Pattern for 5 Transactions

```haskell
-- Using _coinContract as the universal list
stoa0 :: Genesis
stoa0 = Genesis
    { _version     = Stoa
    , _tag         = "Stoa"
    , _txChainIds  = onlyChainId 0
    , _coinbase    = Nothing
    , _keysets     = Nothing
    , _allocations = Nothing
    , _namespaces  = Nothing
    , _coinContract =
        [ "pact/genesis/stoa/stoa-genesis-1.yaml"
        , "pact/genesis/stoa/stoa-genesis-2.yaml"
        , "pact/genesis/stoa/stoa-genesis-3.yaml"
        , "pact/genesis/stoa/stoa-genesis-4-chain0.yaml"
        , "pact/genesis/stoa/stoa-genesis-5.yaml"
        ]
    }

stoaN :: Genesis
stoaN = stoa0
    & txChainIds   .~ mkChainIdRange 1 9
    & coinContract .~
        [ "pact/genesis/stoa/stoa-genesis-1.yaml"
        , "pact/genesis/stoa/stoa-genesis-2.yaml"
        , "pact/genesis/stoa/stoa-genesis-3.yaml"
        , "pact/genesis/stoa/stoa-genesis-4-chains1-9.yaml"
        , "pact/genesis/stoa/stoa-genesis-5.yaml"
        ]
```

---

## Open Questions

1. **`pact/stoa-coin/new-coin.pact` runtime usage**
   - What we know: The coinbase Haskell code (`Pact5/TransactionExec.hs`) pre-installs `COINBASE` and `DEBIT` capabilities. The genesis payload Haskell module contains the encoded transactions.
   - What's unclear: Does the runtime Pact service re-evaluate `pact/stoa-coin/new-coin.pact` from disk for coinbase, or does it use the stored genesis state?
   - Recommendation: Update `new-coin.pact` to match `stoa-genesis-4.pact` anyway as defensive measure. Verify by running a local node after rebuild.

2. **Governance cap acquisition in genesis for `stoic-xchain`**
   - What we know: Phase 3 ran `A_InitialiseStoaChain` (which requires GOVERNANCE) with `keyPairs: []` and it worked.
   - What's unclear: Whether the same pattern works for `stoic-xchain.create-xchain-gas-account` (different module, same pattern).
   - Recommendation: Use `keyPairs: []` (matching Phase 3 pattern). If ea fails on tx5, investigate adding a signatory context.

3. **Gas budget for 5 transactions vs old 4 transactions**
   - What we know: Phase 3 used 300,000 gas budget. The ea tool uses `GasLimit 999_999_999` for genesis (effectively unlimited).
   - What's unclear: Whether the larger genesis (more transactions, more interfaces) stays within production gas limits.
   - Recommendation: Not a concern during ea tool generation (gas is free). At production, genesis runs with special unlimited gas. No action needed.

---

## Sources

### Primary (HIGH confidence — direct code inspection)

- `/Users/codera/Development/Stoa/stoa-chain/cwtools/ea/Ea.hs` — mkPayloads, mkTx, writePayload flow
- `/Users/codera/Development/Stoa/stoa-chain/cwtools/ea/Ea/Genesis.hs` — Genesis record structure, stoa0/stoaN definitions
- `/Users/codera/Development/Stoa/stoa-chain/src/Chainweb/Version/Stoa.hs` — current genesisTime, genesis payload references
- `/Users/codera/Development/Stoa/stoa-chain/src/Chainweb/BlockHeader/Genesis/Stoa0Payload.hs` — current generated payload (gets replaced)
- `/Users/codera/Development/Stoa/stoa-chain/dist-newstyle/.../Pact/ApiReq.hs` — YAML parsing fields (codeFile, dataFile, code, data, nonce, keyPairs)
- `/Users/codera/Development/Stoa/stoa-chain/src/Chainweb/MinerReward.hs` — CSV hash verification (NOT affected by genesis time)
- `/Users/codera/Development/Stoa/stoa-chain/pact/genesis/stoa/` — current genesis YAML files (4 files)
- `/Users/codera/Development/Stoa/genesis_stoa_update/` — all 8 new genesis files from Mihai (5 pact + 3 json + note: stoa-genesis-3 and stoa-genesis-5 have no json)

### Secondary (MEDIUM confidence)

- Project MEMORY.md — Phase 3 Genesis generation was complete; Phase 5 Integration verified gas pipeline, coinbase, 10-chain mining
- PROJECT ROADMAP.md — Phase 6 context and objective
- `/Users/codera/Development/Stoa/stoa-chain/src/Chainweb/Pact/PactService.hs:584` — `execNewGenesisBlock` uses unlimited gas (`999_999_999`) for genesis block production

---

## Metadata

**Confidence breakdown:**
- File locations and content: HIGH — directly verified by reading all source files
- Ea tool flow and YAML format: HIGH — verified from ApiReq.hs and Ea.hs source
- Transaction ordering and dependencies: HIGH — verified from new pact file content
- Governance cap in genesis: MEDIUM — pattern worked in Phase 3; stoa-genesis-5 is a new case
- `new-coin.pact` runtime behavior: MEDIUM — not fully traced through all Pact service paths

**Research date:** 2026-02-23
**Valid until:** 2026-02-26 (before genesis time deadline 2026-02-24T19:00:00Z)

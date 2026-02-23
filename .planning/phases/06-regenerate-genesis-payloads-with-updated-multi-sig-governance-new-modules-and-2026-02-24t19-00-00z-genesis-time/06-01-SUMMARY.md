---
phase: 06-regenerate-genesis-payloads-with-updated-multi-sig-governance-new-modules-and-2026-02-24t19-00-00z-genesis-time
plan: 01
subsystem: genesis
tags: [genesis, pact, yaml, stoa-version]
dependency_graph:
  requires: []
  provides: [stoa-genesis-1..5.pact, stoa-genesis-4-no-init.pact, 6 YAML wrappers, updated Stoa.hs genesis time]
  affects: [cwtools/ea/Ea/Genesis.hs (next step), pact/stoa-coin/new-coin.pact (next step)]
tech_stack:
  added: []
  patterns: [YAML-wrapper pattern for Ea tool, chain-0/chains-1-9 pact file split]
key_files:
  created:
    - pact/genesis/stoa/stoa-genesis-1.pact
    - pact/genesis/stoa/stoa-genesis-1.json
    - pact/genesis/stoa/stoa-genesis-2.pact
    - pact/genesis/stoa/stoa-genesis-2.json
    - pact/genesis/stoa/stoa-genesis-3.pact
    - pact/genesis/stoa/stoa-genesis-4.pact
    - pact/genesis/stoa/stoa-genesis-4.json
    - pact/genesis/stoa/stoa-genesis-4-no-init.pact
    - pact/genesis/stoa/stoa-genesis-5.pact
    - pact/genesis/stoa/stoa-genesis-1.yaml
    - pact/genesis/stoa/stoa-genesis-2.yaml
    - pact/genesis/stoa/stoa-genesis-3.yaml
    - pact/genesis/stoa/stoa-genesis-4-chain0.yaml
    - pact/genesis/stoa/stoa-genesis-4-chains1-9.yaml
    - pact/genesis/stoa/stoa-genesis-5.yaml
  modified:
    - src/Chainweb/Version/Stoa.hs
decisions:
  - "Used dataFile YAML field for JSON data (cleaner than inline data blocks)"
  - "stoa-genesis-4-no-init.pact = lines 1-1657 of stoa-genesis-4.pact (omits foundation keyset define + A_InitialiseStoaChain call)"
  - "GENESIS-TIME updated from 2026-02-18T21:30:00Z to 2026-02-24T19:00:00Z in both pact and Haskell"
metrics:
  duration: ~2 minutes
  completed: 2026-02-23
  tasks_completed: 2
  files_created: 15
  files_modified: 1
---

# Phase 6 Plan 1: Stoa Genesis File Preparation Summary

**One-liner:** 5-transaction genesis architecture with namespaced interfaces, multi-sig governance, util modules, and stoic-xchain — files staged for Ea tool consumption with 2026-02-24T19:00:00Z genesis time.

## Tasks Completed

| Task | Name | Commit | Key Files |
|------|------|--------|-----------|
| 1 | Copy pact files, create chain-0/chains-1-9 split, update GENESIS-TIME | b956219 | stoa-genesis-1..5.pact, stoa-genesis-4-no-init.pact, 3 JSON files |
| 2 | Create 6 YAML wrappers and update Stoa.hs genesis time | 56ba48c | 6 YAML files, src/Chainweb/Version/Stoa.hs |

## What Was Built

### New 5-Transaction Genesis Architecture

Replaced the old 4-file Stoa genesis with Mihai's 5-transaction architecture:

| Tx | File | Content | Chains |
|----|------|---------|--------|
| 1 | stoa-genesis-1.pact | ns module, registry, stoa-ns/ouronet-ns/user/free/util namespaces, root keysets | 0-9 |
| 2 | stoa-genesis-2.pact | fungible-v1/xchain-v1/gas-payer-v1/stoic-fungible-v1 interfaces, stoic-predicates module, 7 stoa_master keysets | 0-9 |
| 3 | stoa-genesis-3.pact | util.guards + util.gas-guards modules (AUTONOMOUS) | 0-9 |
| 4a | stoa-genesis-4.pact | coin module (stoa-ns.fungible-v1 etc.), tables, foundation keyset, A_InitialiseStoaChain | 0 only |
| 4b | stoa-genesis-4-no-init.pact | coin module only (lines 1-1657, no init call) | 1-9 |
| 5 | stoa-genesis-5.pact | stoic-xchain module, kadena-xchain-gas + stoa-xchain-gas accounts | 0-9 |

### YAML Wrappers (6 files)

All YAML files use `codeFile` pointing to same-directory pact files, with `dataFile` for JSON data where needed:

- stoa-genesis-1.yaml: codeFile + dataFile
- stoa-genesis-2.yaml: codeFile + dataFile
- stoa-genesis-3.yaml: codeFile only (no data)
- stoa-genesis-4-chain0.yaml: codeFile + dataFile (full pact with init)
- stoa-genesis-4-chains1-9.yaml: codeFile only (no-init pact, no data)
- stoa-genesis-5.yaml: codeFile only (no data)

### GENESIS-TIME Update (both constants synchronized)

- `stoa-genesis-4.pact` line 84: `(time "2026-02-24T19:00:00Z")` (was `2026-02-18T21:30:00Z`)
- `src/Chainweb/Version/Stoa.hs` `_genesisTime`: `2026-02-24T19:00:00.000000` (was `2026-02-01T00:00:00.000000`)

Both constants now match exactly, ensuring correct year-0 emission calculations and gas price progression.

## Deviations from Plan

None - plan executed exactly as written.

## File Inventory (Final)

```
pact/genesis/stoa/
├── stoa-genesis-1.pact     (ns module + registry)
├── stoa-genesis-1.json     (ns keysets data)
├── stoa-genesis-1.yaml     (YAML wrapper for tx1)
├── stoa-genesis-2.pact     (interfaces + stoic-predicates + keysets 1-7)
├── stoa-genesis-2.json     (master keyset data)
├── stoa-genesis-2.yaml     (YAML wrapper for tx2)
├── stoa-genesis-3.pact     (util.guards + util.gas-guards)
├── stoa-genesis-3.yaml     (YAML wrapper for tx3)
├── stoa-genesis-4.pact     (coin module + init - chain 0)
├── stoa-genesis-4-no-init.pact  (coin module only - chains 1-9)
├── stoa-genesis-4.json     (foundation keyset data)
├── stoa-genesis-4-chain0.yaml   (YAML wrapper for tx4 chain 0)
├── stoa-genesis-4-chains1-9.yaml (YAML wrapper for tx4 chains 1-9)
├── stoa-genesis-5.pact     (stoic-xchain + gas accounts)
└── stoa-genesis-5.yaml     (YAML wrapper for tx5)
```

## Next Steps

1. Update `cwtools/ea/Ea/Genesis.hs`: rewrite stoa0/stoaN records to use 5-element `_coinContract` list
2. Run `cabal run ea` to regenerate Stoa0Payload.hs and Stoa1to9Payload.hs
3. Update `pact/stoa-coin/new-coin.pact` to match stoa-genesis-4.pact (defensive measure)
4. Build `chainweb-node` and deploy to node1.stoachain.com before 2026-02-24T19:00:00Z

## Self-Check: PASSED

Files verified:
- 6 pact files in pact/genesis/stoa/: FOUND
- 3 JSON files in pact/genesis/stoa/: FOUND
- 6 YAML files in pact/genesis/stoa/: FOUND
- GENESIS-TIME in stoa-genesis-4.pact: 2026-02-24T19:00:00Z FOUND
- GENESIS-TIME in stoa-genesis-4-no-init.pact: 2026-02-24T19:00:00Z FOUND
- A_InitialiseStoaChain CALL absent from stoa-genesis-4-no-init.pact: CONFIRMED
- A_InitialiseStoaChain CALL present in stoa-genesis-4.pact: CONFIRMED
- _genesisTime in Stoa.hs: 2026-02-24T19:00:00.000000 FOUND
- All 6 YAML codeFile references resolve to existing pact files: CONFIRMED

Commits verified:
- b956219: feat(06-01): copy Mihai genesis pact files and create chain-0/chains-1-9 split
- 56ba48c: feat(06-01): create 6 YAML wrappers and update Stoa.hs genesis time to 2026-02-24T19:00:00

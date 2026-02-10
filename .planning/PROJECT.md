# Stoa Chain

## What This Is

Stoa Chain is an independent Proof-of-Work parallel-chain blockchain, forked from Kadena's Chainweb Node (v2.32.0). The goal is to launch a new network with custom tokenomics (STOA coin with declining emissions and UR-STOA staking vault), reduced chain count (10 chains instead of 20), and updated gas limits — all achieved by modifying the existing codebase with minimal changes for maximum impact.

## Core Value

The existing Kadena codebase must be surgically modified — not extended with new modules — to produce a working node that boots from a new genesis with STOA's tokenomics, 10 parallel chains, and increased gas capacity.

## Requirements

### Validated

- ✓ Chainweb PoW consensus engine — existing
- ✓ Parallel chain architecture — existing
- ✓ Pact smart contract execution (v4/v5) — existing
- ✓ P2P networking and peer discovery — existing
- ✓ Block header validation and storage — existing
- ✓ Cross-chain SPV proofs — existing
- ✓ Mining coordination — existing
- ✓ RocksDB persistent storage — existing
- ✓ Servant-based REST API — existing
- ✓ Mempool transaction management — existing

### Active

- [ ] Replace existing coin.pact with STOA coin contract (new tokenomics, UR-STOA, vault)
- [ ] Create new genesis transactions (deploy coin module with bundled interfaces, define 7 Stoa Master keysets, call A_InitialiseStoaChain on chain 0)
- [ ] Reduce chain count from 20 to 10 (modify chain graph in Haskell)
- [ ] Increase gas limits (default 400k, maximum 500k — up from 150k/180k)
- [ ] Verify new-coin.pact correctness before deployment
- [ ] Investigate interaction between Haskell gas limits and Pact-side dynamic gas pricing

### Out of Scope

- KDA → STOA string rename in Haskell — cosmetic, zero functional impact, can be done later
- New Haskell modules or features — modify existing only
- Mobile wallet or frontend — blockchain node changes only
- Compatibility with Kadena mainnet — this is a separate network

## Context

- Forked from Kadena Chainweb Node v2.32.0 (Kadena mainnet stopped producing blocks 2025-11-15)
- Haskell codebase with GHC 9.10, Cabal build system
- Multi-package project: chainweb (core lib), chainweb-node (executable), cwtools (utilities), chainweb-storage (storage abstraction)
- Kadena used a 20-chain Petersen graph topology — reducing to 10 chains requires a different graph
- The new coin contract bundles interfaces (fungible-v2, fungible-xchain-v1, gas-payer-v1, StoaFungibleV1) in one file — genesis loading must handle this
- Genesis time defined as 2026-01-01T00:00:00Z in the coin contract
- STOA tokenomics: 16M genesis supply, declining yearly emissions formula, 90% to miners / 10% to UR-STOA vault on chain 0
- UR-STOA: 1M supply staking token on chain 0 only, RPS-based reward distribution
- 7 Stoa Master keysets for governance (must be defined in genesis)
- Existing codebase maps available in `.planning/codebase/`

## Constraints

- **Approach**: Change existing code only — no new modules, no new architectural patterns
- **Minimal changes**: Every modification must be justified; prefer the smallest change that achieves the goal
- **Haskell build**: Must compile with `-Wall -Werror -Wcompat` (all warnings are errors)
- **Chain graph**: 10-chain graph must maintain the braided PoW security properties (adjacent chains validate each other)
- **Genesis compatibility**: The Haskell genesis loading code must handle bundled interfaces + coin module in one file

## Key Decisions

| Decision | Rationale | Outcome |
|----------|-----------|---------|
| Skip KDA→STOA Haskell rename | Cosmetic only, increases risk for zero functional benefit | — Pending |
| Keep interfaces bundled in coin.pact | Simpler deployment, one file. Modify genesis loading to handle it | — Pending |
| 10 chains (decided) | Reduced from 20 — requires new chain graph topology | — Pending |
| Chain 0 special role | Full init (STOA + URSTOA + Vault) on chain 0, coin module only on chains 1-9 | — Pending |

---
*Last updated: 2026-02-10 after initialization*

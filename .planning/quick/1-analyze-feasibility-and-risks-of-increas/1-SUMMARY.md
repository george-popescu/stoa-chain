# Gas Limit Increase Analysis: 400k/500k -> 1.6M/2M

## Current State

| Parameter | Current Value | Location |
|-----------|--------------|----------|
| Default block gas limit | 400,000 | `src/Chainweb/Chainweb/Configuration.hs:456` |
| Max block gas limit | 500,000 | `src/Chainweb/Version/Stoa.hs:56` |

## Proposed Change

| Parameter | Current | Proposed |
|-----------|---------|----------|
| Default block gas limit | 400,000 | 1,600,000 |
| Max block gas limit | 500,000 | 2,000,000 |

## Difficulty: LOW (2-3 lines of code)

### Files to Modify

**1. `src/Chainweb/Version/Stoa.hs` line 56:**
```haskell
-- Current:
, _versionMaxBlockGasLimit = Bottom (minBound, Just 500_000)

-- Option A: Simple change (requires coordinated upgrade, all nodes restart simultaneously)
, _versionMaxBlockGasLimit = Bottom (minBound, Just 2_000_000)

-- Option B: Height-based activation (safe upgrade, nodes can upgrade before activation height)
, _versionMaxBlockGasLimit =
    (BlockHeight TARGET_HEIGHT, Just 2_000_000) `Above`
    Bottom (minBound, Just 500_000)
```

**2. `src/Chainweb/Chainweb/Configuration.hs` line 456:**
```haskell
-- Current:
_configBlockGasLimit = 400_000

-- Change to:
_configBlockGasLimit = 1_600_000
```

**3. Regenerate genesis payloads** (only if Option A — changing from genesis)

## Risk Analysis: Existing Blocks

### Can we break existing blocks?

**Short answer: NO, if done correctly with Option B (height-based activation).**

### How block gas limits work in consensus:

1. Gas limits are **NOT stored in block headers** — they're computed deterministically from `(chainwebVersion, blockHeight)`
2. When validating a block, the node calls `maxBlockGasLimit v blockHeight` to get the limit for that specific height
3. All transactions in the block are **re-executed** during validation (not just hash-checked)

### Option A: Change from genesis (Bottom) — RISKY

If you change `Bottom (minBound, Just 500_000)` to `Bottom (minBound, Just 2_000_000)`:
- Old blocks at height 0-N were created with 500k limit
- Re-validation of old blocks would use the NEW 2M limit
- **This is SAFE because old blocks used LESS gas than the new limit** — a higher limit doesn't invalidate blocks that used less gas
- **BUT**: All nodes must upgrade simultaneously or consensus breaks

### Option B: Height-based activation — SAFE (RECOMMENDED)

Using the existing `Rule` mechanism:
```haskell
_versionMaxBlockGasLimit =
    (BlockHeight 1000, Just 2_000_000) `Above`
    Bottom (minBound, Just 500_000)
```

This means:
- Heights 0-999: max gas = 500,000 (old blocks validated with old limit)
- Heights 1000+: max gas = 2,000,000 (new blocks get new limit)
- **Zero risk to existing blocks** — they're validated with the exact same limit they were created with
- Nodes can upgrade BEFORE the activation height
- After all nodes upgrade, activation happens automatically at the target height

### Validation rule: `ruleValid` requires monotonically non-decreasing limits
- 500k -> 2M is **increasing** = VALID
- The system explicitly prevents decreasing gas limits (`Registry.hs:96-97`)

## Consensus Impact

Gas limits ARE part of consensus:
- Nodes with different gas limits will compute different transaction orderings
- Different gas limits → different "fit" of transactions in blocks
- **ALL nodes must run the same version code** for consensus

### What happens if nodes disagree:
- Node A (500k) receives block created by Node B (2M) that uses 1M gas
- Node A re-executes transactions, hits `BlockGasLimitExceeded` at 500k
- **Node A rejects the block** → chain fork

## Implementation Steps

### For live blockchain (Option B — recommended):

1. Pick a future activation height (e.g., current height + ~2880 blocks = ~1 day buffer)
2. Modify `Stoa.hs`: Add height-based rule
3. Modify `Configuration.hs`: Update default config
4. **Do NOT regenerate genesis** — genesis payloads don't change
5. Build, push, deploy to ALL nodes BEFORE activation height
6. At activation height, new gas limit takes effect automatically

### For fresh chain (Option A — if you're willing to wipe):

1. Modify `Stoa.hs`: Change `Bottom` value
2. Modify `Configuration.hs`: Update default config
3. Regenerate genesis payloads with `ea --stoa`
4. Rebuild
5. Wipe DB, deploy, restart

## Recommendation

**Given that the blockchain is live with blocks mined, use Option B (height-based activation).**

- Zero risk to existing blocks
- No DB wipe needed
- Smooth transition
- Standard upgrade pattern (same as Kadena mainnet used for their gas limit changes)

However, since this is currently a single-node network, Option A is also viable if you're okay with a brief wipe + restart. The chain is very young (low height).

## Timeline Estimate

- Code changes: ~15 minutes
- Build + test: ~30 minutes
- Deploy: ~10 minutes
- Total: Under 1 hour

---
phase: 04-tokenomics
plan: 02
subsystem: tokenomics
tags: [emission-split, vault-injection, repl-tests, 90-10-split, urv-emission]

# Dependency graph
requires:
  - phase: 01-contract-verification
    provides: "Deployed STOA contract with verified coinbase, URC_Emissions, and vault initialization"
  - phase: 04-tokenomics
    plan: 01
    provides: "STOA miner rewards CSV with declining emission schedule"
provides:
  - "Multi-year REPL verification of 90/10 miner/vault emission split (TOKN-04)"
  - "Chain 0 vault injection confirmed at years 0 and 1 (TOKN-05)"
  - "Emission decline verified across years 0, 1, and 10"
affects: [05-integration-testing]

# Tech tracking
tech-stack:
  added: []
  patterns:
    - "Pact REPL range-based assertions for decimal precision: (and (> val lower) (< val upper))"
    - "Exact emission value assertions from verified Pact REPL output"

key-files:
  created: []
  modified:
    - "pact/stoa-coin/stoa-coin.repl"

key-decisions:
  - "Used exact Pact REPL values rather than approximate formulas for emission assertions"
  - "Added vault restriction negative test on chain 5 to strengthen TOKN-05 verification"

patterns-established:
  - "Emission test pattern: set block-time for target year, call URC_Emissions, verify [block-em, urv-em] pair"
  - "90/10 ratio verification: block-em / (block-em + urv-em/10) within 0.01% of 0.9"

# Metrics
duration: 3min
completed: 2026-02-11
---

# Phase 4 Plan 2: Emission Split Verification Summary

**90/10 miner/vault emission split verified at years 0, 1, and 10 via Pact REPL with exact value assertions and declining emission confirmation**

## Performance

- **Duration:** 3 min
- **Started:** 2026-02-11T07:20:25Z
- **Completed:** 2026-02-11T07:23:50Z
- **Tasks:** 1
- **Files modified:** 1

## Accomplishments
- Verified exact emission values at 3 year checkpoints: year 0 (0.414383955844), year 1 (0.407025634069), year 10 (0.348658319546)
- Confirmed 90/10 miner/vault split ratio within 0.01% tolerance at all checkpoints
- Verified chain 0 vault injection works at year 1 (not just year 0), with balance tracking
- Confirmed chain 5 (non-zero) coinbase has no vault interaction, vault operations restricted to chain 0
- Verified emissions decline monotonically: year 10 < year 1 < year 0

## Task Commits

Each task was committed atomically:

1. **Task 1: Add multi-year emission split verification tests** - `6d36c42` (test)

## Files Created/Modified
- `pact/stoa-coin/stoa-coin.repl` - Extended with 5 new test transactions (185 lines) verifying TOKN-04 and TOKN-05

## Decisions Made
- **Used exact Pact REPL values:** Rather than approximate formula-computed values, ran the Pact REPL to get exact output values (e.g., 0.414383955844 for year 0 block-emission) and used those as expected values in assertions. This avoids precision discrepancies between Python/manual calculations and Pact's decimal arithmetic.
- **Added vault restriction negative test:** Beyond verifying miner-only behavior on chain 5, also added an expect-failure test confirming `UR_URV|VaultSupply` is restricted to chain 0, strengthening TOKN-05 verification.

## Deviations from Plan

None - plan executed exactly as written.

## Issues Encountered
None.

## User Setup Required
None - no external service configuration required.

## Next Phase Readiness
- Phase 4 (Tokenomics) is now complete: all TOKN requirements verified
  - TOKN-01: STOA miner rewards CSV created (04-01)
  - TOKN-02: SHA512 hash constants updated (04-01)
  - TOKN-03: Reward divides by 10 via Petersen graph (04-01)
  - TOKN-04: 90/10 split verified at years 0, 1, 10 (04-02)
  - TOKN-05: Vault injection verified at multiple time points (04-02)
- Ready for Phase 5 (Integration Testing)

## Self-Check: PASSED

- FOUND: pact/stoa-coin/stoa-coin.repl
- FOUND: .planning/phases/04-tokenomics/04-02-SUMMARY.md
- FOUND: commit 6d36c42 (Task 1)

---
*Phase: 04-tokenomics*
*Completed: 2026-02-11*

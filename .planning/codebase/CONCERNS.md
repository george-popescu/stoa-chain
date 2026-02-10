# Codebase Concerns

**Analysis Date:** 2026-02-10

## Tech Debt

**Pact Version Migration (Pact 4 → Pact 5):**
- Issue: Significant dual-version maintenance burden with both Pact 4 and Pact 5 implementations coexisting. Hard-coded version forks throughout execution paths.
- Files: `src/Chainweb/Pact/PactService.hs` (lines 498, 570, 895, 949), `src/Chainweb/Pact/RestAPI/Server.hs` (lines 255, 299, 321, 374), `src/Chainweb/Pact5/Types.hs` (line 178)
- Impact: Added complexity in block execution, transaction validation, and API endpoints. Increases maintenance burden and chance of version-specific bugs. Requires careful synchronization of features across versions.
- Fix approach: After Pact 5 transition is complete, systematically remove all Pact 4 execution paths and related code. Create migration guide for any remaining Pact 4 dependencies.

**Orphan Instances (PactDbFor):**
- Issue: Orphan instances for `PactDbFor` type class defined in multiple modules with explicit pragma suppressions.
- Files: `src/Chainweb/Pact4/Backend/ChainwebPactDb.hs` (lines 13-14), `src/Chainweb/Pact5/Backend/ChainwebPactDb.hs` (lines 18-19)
- Impact: Violates Haskell module coherence rules, making instances fragile and subject to import order dependency bugs. Limits modularity.
- Fix approach: Refactor architecture to either (1) define instances in the module where `PactDbFor` is defined, (2) create a dedicated module for instances, or (3) use a different architectural pattern that avoids the instance.

**RocksDB API Abstraction:**
- Issue: `RocksDbTable` API is concrete implementation rather than abstract typeclass.
- Files: `libs/chainweb-storage/src/Chainweb/Storage/Table/RocksDB.hs` (line 37)
- Impact: Difficult to provide alternative storage backends or mock for testing. Tight coupling to RocksDB throughout codebase.
- Fix approach: Create `RocksDbTable` typeclass that abstracts key-value operations. Implement for both RocksDB and in-memory backends.

**Hard-coded Error Messages in Genesis Payloads:**
- Issue: Repeated error message "inconsistent genesis payload detected. THIS IS A BUG in chainweb-node" duplicated across all genesis payload modules without proper structured error handling.
- Files: `src/Chainweb/BlockHeader/Genesis/*.hs` (~30 files, see lines 18, 30 in each)
- Impact: Difficult to distinguish which genesis payload failed. Error message masks underlying validation issue.
- Fix approach: Create centralized genesis validation with detailed error type reporting chain and height information.

**Unsafe Coercions:**
- Issue: `unsafeCoerce` used in singleton instance construction without proper verification.
- Files: `src/Data/Singletons.hs` (line 157)
- Impact: Type safety guarantees are broken. Changes to singleton types could silently compile and cause runtime errors.
- Fix approach: Verify that the coercion is actually safe (it may be for singletons), or refactor to use type-safe alternatives.

**Unsafe Performance IO:**
- Issue: `unsafePerformIO` used in global mutable state initialization for randomness.
- Files: `src/Utils/Logging.hs` (line 394), `src/P2P/Node/PeerDB.hs` (line 214), `cwtools/ea/Ea.hs` (line 335)
- Impact: Hidden side effects in pure code. Makes testing difficult and can cause issues with lazy evaluation and thread safety.
- Fix approach: Use proper IO initialization at application startup rather than hidden side effects.

## Known Bugs

**Broken Test Suites:**
- Symptoms: Multiple test suites marked with BROKEN comment and excluded from main test runs
- Files: `test/unit/ChainwebTests.hs` (lines 130, 134, 146, 161)
  - `Chainweb.Test.Pact4.PactMultiChainTest` — BROKEN: "few tests"
  - `Chainweb.Test.Pact4.VerifierPluginTest` — BROKEN
  - `Chainweb.Test.Pact4.RemotePactTest` — BROKEN
  - `Chainweb.Test.Pact4.TransactionTests` — Awaiting Jose's loadScript function
- Trigger: Running full test suite
- Workaround: Tests are commented/marked but still included in source. Manually excluded from test runs.

**Deprecated and Stubbed PactDb Method:**
- Symptoms: `_getUserTableInfo` in PactDb returns immediate error without implementation
- Files: `src/Chainweb/Pact4/Backend/ChainwebPactDb.hs` (line 277)
- Impact: Any code path calling this method will crash at runtime with "WILL BE DEPRECATED!" error
- Trigger: Call to `getUserTableInfo` on Pact 4 database
- Workaround: Avoid calling this method; it's marked for deprecation

**Flaky Tests (Race Conditions):**
- Symptoms: Timing-sensitive tests in RemotePactTest that use ranges to mitigate flakiness
- Files: `test/unit/Chainweb/Test/Pact4/RemotePactTest.hs` (comments about race and flaky range validation)
- Impact: Tests may occasionally pass/fail unpredictably; blocks reliable CI/CD
- Cause: Polling for block creation has inherent race where block could be created between poll and response
- Workaround: Use small ranges for validation rather than exact timing checks

**Incomplete Pattern Match Warnings:**
- Symptoms: Test suite warns about incomplete pattern matches when new network versions added
- Files: `test/unit/Chainweb/Test/BlockHeader/Genesis.hs` (line 43)
- Impact: Requires developer to manually update test cases when versions added
- Trigger: Adding new `ChainwebVersion`
- Workaround: Add exhaustive tests for all versions at genesis

## Security Considerations

**Hash Algorithm Fallibility:**
- Risk: Manual ad-hoc hashing of binary data without standard algorithms in some paths
- Files: `src/Chainweb/Crypto/MerkleLog.hs` (comment about ad-hoc hashing)
- Current mitigation: Hash verification code exists downstream
- Recommendations: (1) Audit all custom hash code paths, (2) Use only standard hash algorithms, (3) Add tests with known hash vectors

**X509 Certificate Error Handling:**
- Risk: Unrecoverable errors in certificate parsing and serialization silently fail or use `error`
- Files: `src/Network/X509/SelfSigned.hs` (multiple `error` calls on EC point serialization, certificate reading)
- Current mitigation: Used primarily in node startup, not in hot paths
- Recommendations: (1) Use `Either` or `ExceptT` for certificate operations, (2) Provide detailed error messages, (3) Test certificate parsing with malformed inputs

**Serialization of Private Keys:**
- Risk: EC point at infinity cannot be serialized; throws error
- Files: `src/Network/X509/SelfSigned.hs` (line: `error "can't serialize EC point at infinity"`)
- Current mitigation: Point at infinity should theoretically never be used in valid keys
- Recommendations: (1) Validate keys before serialization, (2) Use type system to prevent point at infinity from being created

**Transaction Codec Decoding Errors:**
- Risk: Transaction decoding errors in hot path use generic `error` with decoded transaction UTF8 content
- Files: `src/Chainweb/Mempool/InMem.hs` (line ~650: error with decoded transaction)
- Impact: Malformed transaction could crash mempool processing
- Recommendations: (1) Use proper exception type, (2) Limit transaction content in error messages, (3) Add per-transaction error recovery

## Performance Bottlenecks

**Large Monolithic Files (Complexity):**
- Problem: Several core modules exceed 1400+ lines, making reasoning and modification difficult
- Files:
  - `src/Chainweb/Utils.hs` (1592 lines) — Core utilities, highly polymorphic
  - `src/Chainweb/Pact4/TransactionExec.hs` (1553 lines) — Complex transaction execution logic
  - `src/Chainweb/BlockHeader/Internal.hs` (1323 lines) — Block header validation and type definitions
  - `src/Chainweb/Pact/Types.hs` (1358 lines) — Type definitions and instances
  - `src/Chainweb/Payload.hs` (1435 lines) — Payload handling and storage
- Impact: Difficult to navigate, reason about, and refactor. High risk of introducing bugs during changes.
- Improvement path: (1) Break into smaller focused modules, (2) Extract type definitions into separate module, (3) Create internal submodules for implementation details

**In-Memory Mempool with HashMap:**
- Problem: Full HashMap scan for transaction selection in `getBlockInMem` potentially O(n) where n = total mempool transactions
- Files: `src/Chainweb/Mempool/InMem.hs` (lines 600-700 area)
- Impact: On large mempools (many pending transactions), block creation latency increases linearly with pool size
- Current: HashMap scanned to find and filter transactions; bad transactions scanned separately
- Improvement path: (1) Use priority queue structure for transaction selection, (2) Implement transaction pre-validation, (3) Cache hot transaction sets

**RocksDB Configuration Uncertainty:**
- Problem: Prefix seek setting with unclear semantics and unclear optimization benefit
- Files: `libs/chainweb-storage/src/Chainweb/Storage/Table/RocksDB.hs` (line 457)
- Impact: May be leaving performance on table due to unclear settings
- Improvement path: (1) Document what this setting does, (2) Benchmark with/without, (3) Consider making configurable

**Pact Execution with Force/NFData:**
- Problem: Lazy errors forced at runtime rather than caught earlier
- Files: `src/Chainweb/Pact4/TransactionExec.hs` (line 947: "forcing it here for lazy errors. TODO NFData the Pacts")
- Impact: Some errors only surface during force, not during pure computation
- Improvement path: (1) Add NFData instances to Pact types, (2) Force earlier in pipeline, (3) Add structured error logging

## Fragile Areas

**Transaction Execution Branching (Pact 4 vs Pact 5):**
- Files: `src/Chainweb/Pact/PactService.hs` (entire `SomeBlockM` pattern with `Pair` branches)
- Why fragile: Every change to block execution requires updating both branches. Easy to forget one side.
- Safe modification: (1) Add integration tests for both versions, (2) Extract common logic, (3) Use property-based testing
- Test coverage: Good — both branches executed by test suite

**Cut Validation and Braiding:**
- Files: `src/Chainweb/Cut.hs` (lines 626, 660, 663 — TODO/FIXME comments about cut consistency), `src/Chainweb/BlockHeader/Validation.hs` (line 890)
- Why fragile: Cut consistency is critical for consensus. Multiple incomplete checks and TODOs about proper validation.
- Safe modification: (1) Add comprehensive property tests for cut invariants, (2) Implement formal verification of braiding, (3) Slow but correct path for cut validation
- Test coverage: Test/Cut.hs exists but gaps noted

**TreeDB and Block Header Store:**
- Files: `src/Chainweb/TreeDB.hs` (1075 lines, complex stream logic), `src/Chainweb/Sync/WebBlockHeaderStore.hs` (recursive calls potentially long-running on line 402)
- Why fragile: Complex iteration/streaming logic with potential for infinite loops or resource exhaustion
- Safe modification: (1) Add iteration depth tracking, (2) Add timeout parameters, (3) Property test all iteration patterns
- Test coverage: WebBlockHeaderStore has limited test coverage (line 129 TODO)

**SPV Proof Creation:**
- Files: `src/Chainweb/SPV/CreateProof.hs` (lines 157-267 with multiple TODOs about transaction tree caching)
- Why fragile: Multiple incomplete optimizations and TODO paths. Missing error handling.
- Safe modification: (1) Implement all TODOs, (2) Add detailed error typing, (3) Benchmark proof creation time
- Test coverage: SPV tests exist but use precomputed values (not generated)

## Scaling Limits

**Block Height Encoding Assumptions:**
- Current capacity: Tested to mainnet block heights (2.9M+)
- Limit: BlockHeight uses Word32 internally — maximum 4,294,967,295 blocks
- Scaling path: At current mainnet rate (~15s per block), this provides ~2000+ years of runway
- Concern: Some code uses BlockHeight directly in arithmetic; overflow not always checked

**Mempool Memory Growth:**
- Current capacity: In-memory HashMap with no size limits enforced
- Limit: Available system RAM; growing without bounds as transactions accumulate
- Scaling path: (1) Implement memory limit with eviction policy, (2) Implement transaction TTL pruning, (3) Consider disk-backed mempool

**RocksDB Performance at Scale:**
- Current capacity: Mainnet running successfully; tested paths exist
- Limit: Compaction pauses can stall consensus; disk I/O becomes bottleneck
- Scaling path: (1) Tune compaction strategies, (2) Consider different storage engines, (3) Implement read replicas

**Pact Module Cache:**
- Current capacity: In-memory cache for compiled Pact modules
- Limit: Module cache grows unbounded per chain; no documented eviction
- Scaling path: (1) Implement LRU cache, (2) Add cache size limits, (3) Benchmark typical cache size

## Dependencies at Risk

**GHC Version Pinning:**
- Risk: Build system strongly tied to GHC 9.10 and 9.8 only
- Impact: Difficult to adopt GHC improvements; stuck on older compiler
- Tested-with: `chainweb.cabal` line 18-19
- Migration plan: (1) Test with newer GHC versions, (2) Address `-Wall -Werror` issues, (3) Update dependencies for new GHC

**Pact Library Dependency:**
- Risk: Tight coupling to specific Pact versions (v4, v5 coexisting)
- Impact: Can't update Pact without major refactor
- Current: Both pact4 and pact5 packages as dependencies
- Migration plan: Complete Pact 5 transition, remove Pact 4 dependency entirely

**Haskell Streaming Library:**
- Risk: Streaming library used extensively but has subtle resource semantics
- Impact: Resource leaks possible if streams not properly consumed/closed
- Files: Used in TreeDB, block header sync, SPV proofs
- Mitigation: (1) Use with-style resource handlers, (2) Test for resource leaks

## Missing Critical Features

**Proper Error Recovery in Consensus:**
- Problem: Several code paths use `error` or `fail` for unrecoverable conditions that might be recoverable with better architecture
- Blocks: Graceful handling of temporary storage failures, network partitions
- Files: `src/Network/X509/SelfSigned.hs`, `src/Chainweb/SPV/RestAPI/Server.hs` (FIXMEs about error handling)

**Transaction History Indexing:**
- Problem: Historical transaction lookups potentially expensive; no TODO for optimizations
- Blocks: Complex queries on block transaction history
- Files: `src/Chainweb/Pact/RestAPI/Server.hs` (line 645 TODO about grouping by block for performance)

**Request Timeout Handling:**
- Problem: Mempool operations missing timeout support
- Blocks: Long-running mempool operations could stall node
- Files: `src/Chainweb/Mempool/RestAPI/Client.hs` (line 46 TODO)

## Test Coverage Gaps

**SPV Event Proof Tests:**
- What's not tested: Full end-to-end SPV proof generation with randomized inputs
- Files: `test/unit/Chainweb/Test/SPV.hs` (line 80 — FIXME: tests are randomized, should be rewritten)
- Risk: SPV proofs could fail silently in production for edge cases
- Priority: Medium — SPV is critical for cross-chain features

**Block Header Validation Corner Cases:**
- What's not tested: Real mainnet fork blocks with interesting properties
- Files: `test/unit/Chainweb/Test/BlockHeader/Validation.hs` (line 716 TODO)
- Risk: Consensus failures on edge case blocks
- Priority: High — affects consensus safety

**Pact4 Remote Testing:**
- What's not tested: Full RemotePactTest marked BROKEN (not running)
- Files: `test/unit/Chainweb/Test/Pact4/RemotePactTest.hs`
- Risk: Remote Pact operations untested; could fail in production
- Priority: High — remote Pact operations used in production

**P2P Peer Discovery:**
- What's not tested: Bootstrap configuration edge cases
- Files: `test/lib/Chainweb/Test/P2P/Peer/BootstrapConfig.hs` (line 11 TODO)
- Risk: Peer discovery failures on startup
- Priority: Medium — affects network bootstrap

**Certificate Generation:**
- What's not tested: Invalid/malformed certificate handling
- Files: `test/remote/Network/X509/SelfSigned/Test.hs` (line 13, 134 TODO)
- Risk: Certificate errors crash node
- Priority: Medium — affects TLS setup

---

*Concerns audit: 2026-02-10*

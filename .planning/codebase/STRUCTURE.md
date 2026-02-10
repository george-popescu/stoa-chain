# Codebase Structure

**Analysis Date:** 2026-02-10

## Directory Layout

```
stoa-chain/
├── src/                              # Core library source code (235 .hs files)
│   ├── Chainweb/                     # Consensus and blockchain logic
│   │   ├── BlockHeader/              # Block header types, validation, genesis
│   │   ├── BlockHeaderDB/            # Block header persistence and queries
│   │   ├── BlockHeaderDB/RestAPI/    # BlockHeaderDB HTTP API
│   │   ├── Chainweb/                 # Component orchestration, configuration
│   │   ├── Crypto/                   # Merkle trees, hashing utilities
│   │   ├── Cut/                      # Cross-chain consensus state representation
│   │   ├── CutDB/                    # Cut persistence and queries
│   │   ├── CutDB/RestAPI/            # CutDB HTTP API
│   │   ├── Logging/                  # Component-specific logging config
│   │   ├── Mempool/                  # Transaction pool management
│   │   ├── Mempool/RestAPI/          # Mempool HTTP API (submit, check)
│   │   ├── Miner/                    # Block creation and PoW mining
│   │   ├── Miner/RestAPI/            # Mining HTTP API
│   │   ├── Pact/                     # Unified smart contract service layer
│   │   ├── Pact/Backend/             # Pact state storage, snapshots
│   │   ├── Pact/Backend/PactState/   # SQLite state, EmbeddedSnapshot, GrandHash
│   │   ├── Pact/Backend/SQLite/      # SQLite schema and queries
│   │   ├── Pact/PactService/         # Queue-based service, Checkpointer
│   │   ├── Pact/PactService/Pact4/   # Pact4-specific execution
│   │   ├── Pact/PactService/Pact5/   # Pact5-specific execution
│   │   ├── Pact/RestAPI/             # Pact HTTP API (send, poll, listen)
│   │   ├── Pact4/                    # Pact 4 smart contract engine
│   │   ├── Pact4/Backend/            # Pact4 state database
│   │   ├── Pact5/                    # Pact 5 smart contract engine
│   │   ├── Pact5/Backend/            # Pact5 state database
│   │   ├── Payload/                  # Block payload (transactions + outputs)
│   │   ├── Payload/PayloadStore/     # Payload persistence
│   │   ├── Payload/RestAPI/          # Payload HTTP API
│   │   ├── RestAPI/                  # Shared REST API utilities, health, nodeinfo
│   │   ├── SPV/                      # Simple Payment Verification proofs
│   │   ├── SPV/RestAPI/              # SPV HTTP API
│   │   ├── Store/                    # Storage scheme documentation (README.md)
│   │   ├── Sync/                     # Block synchronization logic
│   │   ├── Utils/                    # Utilities: rankings, fork state, etc.
│   │   ├── VerifierPlugin/           # Pluggable cross-chain verifiers
│   │   ├── VerifierPlugin/Hyperlane/ # Hyperlane message verification
│   │   ├── Version/                  # Network version management (mainnet, testnet, devnets)
│   │   └── [root .hs files]          # Core types: Block, BlockHash, BlockHeight, etc.
│   ├── P2P/                          # Peer-to-peer networking
│   │   ├── Node/                     # P2P node implementation
│   │   ├── Node/RestAPI/             # P2P HTTP API (peer discovery)
│   │   ├── [root .hs files]          # Peer database, bootstrap, sessions
│   ├── Data/                         # Generic data structures (Word types)
│   ├── Network/                      # Network utilities
│   │   └── X509/                     # TLS and certificate handling
│   ├── Numeric/                      # Numeric operations and utilities
│   └── Utils/                        # Cross-cutting utilities
│       └── Logging/                  # Structured logging utilities
├── node/                             # Chainweb node executable
│   ├── src/
│   │   └── ChainwebNode.hs           # Main entry point, orchestration, monitoring
│   ├── Utils/                        # Node utilities (signal handling, limits)
│   └── chainweb-node.cabal           # Node executable build config
├── libs/                             # Library packages
│   └── chainweb-storage/             # Pluggable storage abstraction
│       ├── src/Chainweb/Storage/
│       │   ├── Table.hs              # Storage interface (get, put, delete, batch)
│       │   ├── Table/RocksDB.hs      # RocksDB implementation
│       │   ├── Table/HashMap.hs      # In-memory HashMap implementation
│       │   ├── Table/Forgetful.hs    # Ephemeral storage (for testing)
│       │   └── DedupStore.hs         # Deduplication wrapper
│       └── chainweb-storage.cabal    # Storage library build config
├── cwtools/                          # Command-line utilities
│   ├── compact/                      # Database compaction tool
│   ├── db-checksum/                  # Checksum verification
│   ├── encode-decode/                # Encoding/decoding test utility
│   ├── genconf/                      # Configuration generation
│   ├── header-dump/                  # Block header inspection
│   ├── known-graphs/                 # Graph analysis utilities
│   ├── pact-diff/                    # Pact state comparison
│   ├── run-nodes/                    # Multi-node local test runner
│   ├── txstream/                     # Transaction streaming utility
│   └── cwtools.cabal                 # Tools build config
├── pact/                             # Smart contract code and genesis
│   ├── coin-contract/                # Coin contract implementations (v1-v5)
│   ├── coin-contract/v1/             # Coin contract v1
│   ├── coin-contract/v2/             # Coin contract v2
│   ├── coin-contract/v3/             # Coin contract v3
│   ├── coin-contract/v4/             # Coin contract v4
│   ├── coin-contract/v5/             # Coin contract v5
│   ├── genesis/                      # Genesis block definitions
│   ├── genesis/devnet/               # Devnet genesis (keys.yaml)
│   └── [.pact, .repl, .yaml files]   # Pact code and test data
├── test/                             # Test suites
│   ├── unit/                         # Unit tests (test/unit/Chainweb/Test/...)
│   ├── lib/                          # Shared test infrastructure and fixtures
│   ├── compaction/                   # Database compaction tests
│   ├── multinode/                    # Multi-node network tests
│   ├── pact/                         # Pact-specific tests
│   ├── remote/                       # Remote database tests
│   ├── golden/                       # Golden file comparisons
│   └── [cabal test targets]          # Defined in chainweb.cabal
├── bench/                            # Benchmarks
│   └── Chainweb/                     # Benchmark suites
├── allocations/                      # Initial token allocations CSV
├── rewards/                          # Miner rewards CSV
├── changes/                          # Changelog and version history
├── docs/                             # Documentation and guides
├── scripts/                          # Utility scripts (building, deployment)
├── chainweb.cabal                    # Main library build config (235 .hs files)
├── cabal.project                     # Workspace config (4 packages)
├── flake.nix                         # Nix flake definition
├── default.nix                       # Nix derivation
├── Dockerfile                        # Container image definition
├── README.md                         # Project overview
├── CHANGELOG.md                      # Version history
└── LICENSE                           # BSD-3-Clause license
```

## Directory Purposes

**`src/Chainweb/`:**
- Purpose: Core blockchain consensus logic, block validation, cross-chain state coordination
- Contains: 20+ subdirectories covering specific subsystems
- Key files: `BlockHeader.hs`, `Payload.hs`, `Cut.hs`, `CutDB.hs`, `Chainweb.hs`

**`src/Chainweb/BlockHeader/`:**
- Purpose: Block header types and validation
- Contains: Block header AST, proof-of-work validation, fork state tracking
- Key files: `Internal.hs` (mutable constructors), `Validation.hs` (validation rules)

**`src/Chainweb/BlockHeaderDB/`:**
- Purpose: Persistent storage and querying of block headers per chain
- Contains: Merkle tree structure, database operations, synchronization support
- Key files: `BlockHeaderDB.hs` (main API), `RestAPI/` (HTTP handlers)

**`src/Chainweb/Chainweb/`:**
- Purpose: Component orchestration, resource lifecycle management, configuration
- Contains: `with*/run*` patterns for all major subsystems
- Key files: `Chainweb.hs` (orchestration), `Configuration.hs` (config parsing), `CutResources.hs`, `ChainResources.hs`

**`src/Chainweb/CutDB/`:**
- Purpose: Maintain and query current consensus state across all chains
- Contains: Cut management, cut history, block-to-cut mapping
- Key files: `CutDB.hs` (main API), `RestAPI/` (HTTP handlers)

**`src/Chainweb/Mempool/`:**
- Purpose: In-memory transaction pool, consensus integration
- Contains: Transaction storage, validation integration, reintroduction on forks
- Key files: `InMem.hs` (primary impl), `Consensus.hs` (fork handling), `RestAPI/` (HTTP endpoints)

**`src/Chainweb/Miner/`:**
- Purpose: Block creation and proof-of-work mining coordination
- Contains: Mining loop, difficulty adjustment, mining pool support
- Key files: `Coordinator.hs` (mining orchestration), `Core.hs` (mining math), `RestAPI/` (work requests)

**`src/Chainweb/Pact/`:**
- Purpose: Unified smart contract service, transaction validation, state checkpointing
- Contains: Queue-based service, state snapshots, transaction templates
- Key files: `PactService.hs` (queue API), `Backend/` (state storage), `RestAPI/` (send/poll/listen)

**`src/Chainweb/Pact4/` and `src/Chainweb/Pact5/`:**
- Purpose: Version-specific smart contract engine implementations
- Contains: Transaction execution, validation rules, state database backends
- Key files: `TransactionExec.hs` (execution), `Types.hs` (types), `Backend/` (state)

**`src/Chainweb/Payload/`:**
- Purpose: Block transaction list and execution outputs
- Contains: Payload encoding/decoding, merkle tree, verification
- Key files: `Payload.hs` (types), `PayloadStore/` (persistence), `RestAPI/` (HTTP)

**`src/Chainweb/SPV/`:**
- Purpose: Simple Payment Verification proofs for cross-chain settlement
- Contains: Proof generation and verification, transaction/output proofs, event proofs
- Key files: `CreateProof.hs`, `VerifyProof.hs`, `EventProof.hs`, `OutputProof.hs`

**`src/Chainweb/Version/`:**
- Purpose: Network version management and chain configuration
- Contains: Mainnet, Testnet, Devnet definitions; chain IDs, genesis blocks
- Key files: `Mainnet.hs`, `Testnet.hs`, `Registry.hs`

**`src/P2P/`:**
- Purpose: Peer-to-peer networking for block synchronization and gossip
- Contains: Peer database, bootstrap node discovery, session management
- Key files: `Node.hs` (P2P node), `Peer.hs` (peer types), `RestAPI/` (peer API)

**`libs/chainweb-storage/`:**
- Purpose: Pluggable key-value storage abstraction
- Contains: RocksDB, HashMap, and Forgetful backends; unified interface
- Key files: `Table.hs` (interface), `Table/RocksDB.hs`, `Table/HashMap.hs`

**`node/src/`:**
- Purpose: Application entry point and orchestration
- Contains: Configuration loading, resource initialization, monitoring loops
- Key files: `ChainwebNode.hs` (main module), `Utils/` (helpers)

**`test/`:**
- Purpose: Test suites for all components
- Contains: Unit tests (mirrors src structure), integration tests, golden tests
- Key files: `unit/Chainweb/Test/` (unit tests by subsystem), `lib/` (test fixtures and utilities)

**`cwtools/`:**
- Purpose: Command-line utilities for node administration
- Contains: Database tools, configuration generation, debugging utilities
- Subdirectories: `compact/`, `db-checksum/`, `genconf/`, `header-dump/`, `pact-diff/`, `run-nodes/`, `txstream/`

**`pact/`:**
- Purpose: Smart contract source code and genesis definitions
- Contains: Coin contract (v1-v5), genesis blocks per network, REPL tests
- Key directories: `coin-contract/v1/` through `v5/`, `genesis/`

## Key File Locations

**Entry Points:**
- `node/src/ChainwebNode.hs`: Node executable entry point (main module)
- `src/Chainweb/Chainweb.hs`: Core orchestration (withChainweb, runChainweb functions)
- `src/Chainweb/RestAPI.hs`: REST API aggregation (combines all component APIs)

**Configuration:**
- `node/src/ChainwebNode.hs`: Node config types and parsing
- `src/Chainweb/Chainweb/Configuration.hs`: Chainweb configuration
- `src/Chainweb/Logging/Config.hs`: Logging configuration
- `src/Chainweb/Version/Registry.hs`: Network version registry

**Core Logic:**
- `src/Chainweb/BlockHeader.hs`: Block header interface (read-only getters)
- `src/Chainweb/BlockHeader/Internal.hs`: Block header implementation (mutable)
- `src/Chainweb/Cut.hs`: Cut representation and operations
- `src/Chainweb/Payload.hs`: Block payload types and verification
- `src/Chainweb/CutDB.hs`: Cut database operations

**Testing:**
- `test/unit/Chainweb/Test/`: Unit tests mirroring `src/Chainweb/` structure
- `test/lib/Chainweb/Test/`: Shared test fixtures and utilities
- `test/lib/Chainweb/Test/Orphans.hs`: QuickCheck instances
- `test/lib/Chainweb/Test/Utils/`: Test helpers

**Smart Contracts:**
- `pact/coin-contract/v4/*.pact`: Coin contract v4 source
- `pact/coin-contract/v5/*.pact`: Coin contract v5 source
- `pact/genesis/devnet/keys.yaml`: Devnet genesis keys

## Naming Conventions

**Files:**
- `*.hs`: Haskell source files
- Module hierarchy matches directory structure (e.g., `src/Chainweb/BlockHeader/Internal.hs` → `module Chainweb.BlockHeader.Internal`)
- Internal implementation: Directories with `Internal.hs` module contain encapsulated details
- RestAPI modules: Separate `RestAPI/` subdirectories for HTTP handler code

**Directories:**
- CamelCase for logical subsystems (e.g., `BlockHeader`, `CutDB`, `Mempool`)
- Lowercase for generic utilities (`data`, `network`, `numeric`)
- Plural for collections (e.g., `Allocations`, `Rewards`, `Changes`)

**Modules:**
- Public interface exported in parent module (e.g., `Chainweb.BlockHeader`)
- Internal details in `.Internal` modules (e.g., `Chainweb.BlockHeader.Internal`)
- REST API handlers in `.RestAPI.Server` (e.g., `Chainweb.BlockHeaderDB.RestAPI.Server`)
- REST API clients in `.RestAPI.Client` (e.g., `Chainweb.BlockHeaderDB.RestAPI.Client`)
- Version-specific code in version module (e.g., `Chainweb.Pact4.*`, `Chainweb.Pact5.*`)

**Functions:**
- Resource management: `withComponentName` (allocate and manage lifetime), `runComponentName` (start operation)
- Data access: lowercase `get*`, `lookup*`, `query*`
- Mutations: `add*`, `update*`, `remove*`
- Constructors: `new*`, `mk*`, `from*`
- Predicates: `is*`, `has*`, `check*`

## Where to Add New Code

**New Consensus/Blockchain Feature:**
- Primary code: `src/Chainweb/NewFeature.hs` or `src/Chainweb/ComponentName/` (subdirectory if significant)
- HTTP API: `src/Chainweb/NewFeature/RestAPI.hs` and `src/Chainweb/NewFeature/RestAPI/Server.hs`
- Integrate with orchestration: Update `src/Chainweb/Chainweb.hs` with `with*/run*` functions
- Tests: `test/unit/Chainweb/Test/NewFeature.hs` or `test/lib/Chainweb/Test/NewFeature/` (if extensive)

**New Pact-related Feature:**
- Pact4 impl: `src/Chainweb/Pact4/NewFeature.hs` or modify existing files
- Pact5 impl: `src/Chainweb/Pact5/NewFeature.hs` (mirror Pact4 if applicable)
- Shared logic: `src/Chainweb/Pact/NewFeature.hs` if both versions need it
- Tests: `test/unit/Chainweb/Test/Pact4/` or `test/lib/Chainweb/Test/Pact4/`

**New P2P Networking Feature:**
- Implementation: `src/P2P/NewFeature.hs` or `src/P2P/Node/NewFeature.hs`
- REST API: `src/P2P/Node/RestAPI/Server.hs` (extend existing)
- Tests: `test/unit/P2P/Test/` or `test/lib/P2P/Test/`

**New Command-Line Tool:**
- Tool code: `cwtools/my-tool/Main.hs`
- Create `cwtools/my-tool/` directory with cabal file
- Add to `cabal.project` packages list
- Tests (if complex): `cwtools/my-tool/test/`

**New Utilities (Cross-Cutting):**
- Generic utilities: `src/Utils/NewUtility.hs`
- Data structures: `src/Data/NewType.hs`
- Network utilities: `src/Network/NewFeature.hs`
- Chainweb-specific helpers: `src/Chainweb/Utils.hs` (append if small) or `src/Chainweb/Utils/NewModule.hs`

## Special Directories

**`test/unit/`:**
- Purpose: Unit tests co-located by subsystem
- Generated: No (checked in)
- Committed: Yes
- Structure: Mirrors `src/` hierarchy under `Chainweb/Test/`

**`test/lib/`:**
- Purpose: Shared test fixtures, generators, orphan instances
- Generated: No
- Committed: Yes
- Key modules: `Chainweb/Test/Orphans.hs` (QuickCheck instances), `Chainweb/Test/Utils/` (helpers)

**`test/golden/`:**
- Purpose: Expected output for golden file tests
- Generated: Automatically by tests (can be regenerated)
- Committed: Yes (check in to track expected behavior)
- Files: `.txt` files with serialized test data

**`.planning/codebase/`:**
- Purpose: GSD codebase analysis documents
- Generated: By `/gsd:map-codebase` command
- Committed: Yes (tracks architecture decisions)
- Files: ARCHITECTURE.md, STRUCTURE.md, CONVENTIONS.md, TESTING.md, CONCERNS.md, STACK.md, INTEGRATIONS.md

**`dist-newstyle/`, `.cabal-sandbox/`:**
- Purpose: Build artifacts and cached dependencies
- Generated: Yes
- Committed: No (in .gitignore)

---

*Structure analysis: 2026-02-10*

# Architecture

**Analysis Date:** 2026-02-10

## Pattern Overview

**Overall:** Modular multi-layer blockchain node architecture with clear separation between consensus logic, smart contract execution, storage, and networking.

**Key Characteristics:**
- Resource-managed component initialization using `with*`/`run*` pattern for safe lifecycle management
- Separate data model layers: block headers, payloads, cuts (cross-chain consensus state)
- Pluggable storage backends (RocksDB, HashMap, Forgetful) via `Chainweb.Storage.Table` abstraction
- Multiple smart contract engine versions (Pact4, Pact5) running in parallel with shared infrastructure
- Type-safe REST API definitions using Servant framework with auto-generated client/server code
- Composition of independent subsystems with mutual dependencies resolved at startup

## Layers

**Core Consensus (`src/Chainweb/`):**
- Purpose: Blockchain consensus, block validation, cross-chain state coordination
- Location: `src/Chainweb/`
- Contains: Block headers, cuts, cut database, payload handling, version management
- Depends on: Storage backends, Crypto libraries, Pact engines
- Used by: Node application, P2P layer, REST API

**Smart Contract Execution (`src/Chainweb/Pact*/`):**
- Purpose: Execute transactions and smart contracts, maintain state snapshots, gas metering
- Location: `src/Chainweb/Pact/`, `src/Chainweb/Pact4/`, `src/Chainweb/Pact5/`
- Contains: Transaction validation, execution engines, state snapshots, SPV proof creation
- Depends on: Pact language runtime, Storage backends, Payload types
- Used by: Block creation, Mempool validation, REST API endpoints

**Mempool (`src/Chainweb/Mempool/`):**
- Purpose: Transaction pool management, consensus tracking, transaction validation
- Location: `src/Chainweb/Mempool/`
- Contains: In-memory transaction storage, reintroduction logic, validators
- Depends on: Pact validators, Payload types
- Used by: Block creation, Mining, REST API

**Mining (`src/Chainweb/Miner/`):**
- Purpose: Block creation, proof-of-work coordination, mining pool support
- Location: `src/Chainweb/Miner/`
- Contains: Coordinator, core mining logic, Pact integration
- Depends on: CutDB, Mempool, Payload store, Block headers
- Used by: REST API, Node startup

**Networking (`src/P2P/`):**
- Purpose: Peer-to-peer communication, peer discovery, bootstrap node handling
- Location: `src/P2P/`
- Contains: Peer database, session management, task queues, REST API
- Depends on: Networking libraries, TLS support
- Used by: Block synchronization, cut synchronization, cut monitoring

**Storage (`libs/chainweb-storage/src/`, integrated via `src/Chainweb/Storage.Table.*`):**
- Purpose: Pluggable key-value storage abstraction
- Location: `libs/chainweb-storage/src/Chainweb/Storage/Table/`
- Contains: RocksDB backend, HashMap (in-memory), Forgetful (ephemeral), abstraction traits
- Depends on: rocksdb-haskell, standard Haskell libraries
- Used by: BlockHeaderDb, CutDB, Payload store, Pact state

**REST API (`src/Chainweb/RestAPI/`, component-specific `RestAPI` modules):**
- Purpose: HTTP API server definitions and handlers
- Location: `src/Chainweb/RestAPI/`, `src/Chainweb/BlockHeaderDB/RestAPI/`, `src/Chainweb/CutDB/RestAPI/`, etc.
- Contains: Servant API definitions, server handlers, health checks, node info
- Depends on: Component-specific modules, Servant framework
- Used by: Node application HTTP server

**Node Application (`node/src/ChainwebNode.hs`):**
- Purpose: Application entry point, resource orchestration, monitoring
- Location: `node/src/ChainwebNode.hs`
- Contains: Configuration parsing, logger setup, component bootstrapping
- Depends on: All core modules, logging infrastructure
- Used by: Executable entry point

**Utilities (`src/Utils/`, `src/Chainweb/Utils.hs`, `src/Data/`, `src/Numeric/`, `src/Network/`):**
- Purpose: Cross-cutting utilities: logging, encoding/decoding, numeric operations, TLS
- Location: `src/Utils/`, `src/Chainweb/Utils.hs`, `src/Data/`, `src/Numeric/`, `src/Network/`
- Contains: JSON encoding, byte operations, time utilities, X.509 certificate handling
- Depends on: Standard and third-party libraries
- Used by: All layers

## Data Flow

**Block Creation and Synchronization Flow:**

1. Mempool receives new transactions → validators check via Pact
2. Miner polls Mempool and CutDB for current consensus state
3. Miner creates new block with transactions, computes proof-of-work
4. New block header added to BlockHeaderDb, payload to PayloadStore
5. CutDB updates its cut (cross-chain consensus view) to include new block
6. New cut distributed to peers via P2P network
7. Remote nodes receive cut, fetch missing block headers/payloads, validate, update local CutDB

**State Transition Flow (per block execution):**

1. Block header with new block payload hash arrives at CutDB
2. CutDB requests PayloadStore for payload data
3. Pact service (Pact4 or Pact5) executes all transactions in payload
4. Transaction execution updates Pact state DB (SQLite)
5. Output hash computed and verified against header's payload hash
6. State checkpoint written to snapshot store (EmbeddedSnapshot/GrandHash)
7. Block considered valid and immutable

**State Management:**

- **BlockHeaderDb** (`src/Chainweb/BlockHeaderDB.hs`): Merkle tree of block headers per chain, supports efficient syncing
- **CutDB** (`src/Chainweb/CutDB.hs`): Maintains current cut (one header per chain at consistent height)
- **Payload storage** (`src/Chainweb/Payload/PayloadStore/`): Transactions and execution outputs, separated from headers for efficiency
- **Pact state** (`src/Chainweb/Pact/Backend/PactState/`, SQLite): Smart contract state snapshots keyed by block hash

## Key Abstractions

**Cut:**
- Purpose: Represents a consistent cross-chain consensus state across all 20 parallel chains
- Examples: `src/Chainweb/Cut.hs`
- Pattern: Map from ChainId to BlockHeader at a specific height; validated for "braiding" (all blocks in cut are mutually reachable)

**BlockHeader:**
- Purpose: Immutable block metadata: height, parent, transactions hash, outputs hash, proof-of-work, adjacent chain hashes
- Examples: `src/Chainweb/BlockHeader.hs`, `src/Chainweb/BlockHeader/Internal.hs`
- Pattern: Read-only getters via `Chainweb.BlockHeader`; mutable construction only in `Chainweb.BlockHeader.Internal` (for tests)

**Payload:**
- Purpose: Transaction list and execution outputs, stored separately from headers
- Examples: `src/Chainweb/Payload.hs`, `src/Chainweb/Payload/PayloadStore.hs`
- Pattern: `BlockPayload` = `BlockTransactions` + `BlockOutputs`; verified against hashes in header

**SPV (Simple Payment Verification):**
- Purpose: Cross-chain proof that transaction occurred and was finalized
- Examples: `src/Chainweb/SPV/` (CreateProof, VerifyProof, EventProof, OutputProof)
- Pattern: Merkle proofs linking transaction to cut state; verifiable without downloading full blocks

**Verifier Plugin:**
- Purpose: Pluggable verification of external data (e.g., Hyperlane cross-chain messages)
- Examples: `src/Chainweb/VerifierPlugin/Hyperlane/` (Announcement, Message), `src/Chainweb/VerifierPlugin/Allow.hs`
- Pattern: Registry of plugins loaded per chain version; executed during transaction validation

**TreeDB:**
- Purpose: Generic tree structure for efficient ancestor lookup (used internally by BlockHeaderDb)
- Examples: `src/Chainweb/TreeDB.hs`
- Pattern: O(log N) ancestor queries; supports pruning of old branches

## Entry Points

**Node Executable:**
- Location: `node/src/ChainwebNode.hs` (main module: `Main`)
- Triggers: System startup via `cabal run chainweb-node` or compiled binary
- Responsibilities:
  1. Parse configuration file and CLI arguments
  2. Initialize logger and monitoring
  3. Load network version (Mainnet, Testnet, Devnet)
  4. Set up RocksDB and Pact state directories
  5. Initialize all components via `withChainweb` pattern
  6. Start HTTP servers (chainweb API, service API)
  7. Run block monitoring and cut monitoring loops

**REST API Server:**
- Location: `src/Chainweb/RestAPI.hs`
- Triggers: Node startup initiates HTTP server binding
- Responsibilities:
  1. Combine all component APIs (BlockHeaderDB, CutDB, Mempool, Mining, Pact, SPV, P2P)
  2. Serve HTTP requests on configured port (default 8080)
  3. Forward requests to appropriate component handlers

**Cut Monitor:**
- Location: `node/src/ChainwebNode.hs` (`runCutMonitor` function)
- Triggers: Started at node startup in separate thread
- Responsibilities: Periodically log current cut state and block statistics

**Block Update Monitor:**
- Location: `node/src/ChainwebNode.hs` (`runBlockUpdateMonitor` function)
- Triggers: Started at node startup in separate thread
- Responsibilities: Stream new blocks and orphaned blocks, log statistics

## Error Handling

**Strategy:** Composable exception handling with recovery; distinguishes fatal errors (restart) from transient errors (retry with backoff).

**Patterns:**

- **Resource cleanup**: All component initialization uses `with*` pattern (Haskell's `bracket` idiom) to guarantee cleanup on exception
  - Example: `withChainweb` in `src/Chainweb/Chainweb.hs` ensures all resources freed on error

- **Monitor loop recovery**: `runMonitorLoop` catches exceptions, logs them, and restarts monitor threads with exponential backoff
  - Location: `node/src/ChainwebNode.hs`
  - 10 immediate restarts allowed; after that, throttle to 1 restart per 10 seconds

- **Block validation errors**: Invalid blocks rejected with detailed logging; do not affect node consensus
  - Validation happens in `Chainweb.BlockHeader.Validation` before block acceptance

- **Pact transaction errors**: Failed transaction execution logged but included in block; state rolled back if output hash mismatch
  - Location: `src/Chainweb/Pact/PactService/` (Pact4/Pact5 specific)

- **Mempool validation**: Transactions rejected without acceptance if validation fails; logged with reason
  - Validators located in `src/Chainweb/Mempool/` and `src/Chainweb/Pact*/Validations.hs`

- **Storage errors**: RocksDB access failures propagate as `IO` exceptions; caught by monitor loops
  - Storage abstraction in `libs/chainweb-storage/src/Chainweb/Storage/Table.hs`

## Cross-Cutting Concerns

**Logging:**
- Framework: `yet-another-logger` (structured logging)
- Configuration: `src/Chainweb/Logging/Config.hs` and `src/Utils/Logging/Config.hs`
- Entry point: Logger passed through component initialization chain; labeled by component
- Example: Block header database logs in `src/Chainweb/BlockHeaderDB.hs` with component label "blockheaderdb"

**Validation:**
- Block header validation: `src/Chainweb/BlockHeader/Validation.hs` (PoW, merkle proofs, parent links)
- Transaction validation: `src/Chainweb/Pact4/Validations.hs`, `src/Chainweb/Pact5/Validations.hs` (signature checks, gas)
- Payload validation: `src/Chainweb/Payload.hs` (merkle tree verification)

**Authentication:**
- P2P peers: TLS certificates; self-signed for development, managed by operators for production
- REST API: No built-in auth; intended for internal network or proxy authentication layer
- Implementation: `src/Network/X509/SelfSigned.hs` for test certificate generation

---

*Architecture analysis: 2026-02-10*

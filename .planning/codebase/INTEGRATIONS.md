# External Integrations

**Analysis Date:** 2026-02-10

## APIs & External Services

**Smart Contract Execution:**
- Pact (v4) - Smart contract VM for transaction execution
  - Package: `pact 4.2.0.1+` from `https://github.com/kadena-io/pact.git`
  - Purpose: Primary smart contract interpreter for contracts, coin, and custom code
  - Integration: `Chainweb.Pact4.TransactionExec` (`src/Chainweb/Pact4/TransactionExec.hs`)

- Pact (v5) - Next-generation smart contract VM
  - Package: `pact-tng` (v5) from `https://github.com/kadena-io/pact-5.git`
  - Purpose: Enhanced transaction processing, gas metering, plugin verifiers
  - Integration: `Chainweb.Pact5.TransactionExec` (`src/Chainweb/Pact5/TransactionExec.hs`)

**Ethereum Integration & Bridge:**
- Ethereum SDK (`ethereum:{ethereum, secp256k1}` 0.1+)
  - Source: `https://github.com/kadena-io/kadena-ethereum-bridge.git`
  - Purpose: Ethereum SPV verification, cross-chain proof validation
  - Integration:
    - `Chainweb.Pact4.SPV` (`src/Chainweb/Pact4/SPV.hs`) - Ethereum header/receipt parsing
    - `Chainweb.Pact.RestAPI.EthSpv` (`src/Chainweb/Pact/RestAPI/EthSpv.hs`) - SPV REST endpoint
  - Auth: None (data-only integration)

**Cross-Chain Message Verification:**
- Hyperlane Message Plugin - Verifies inter-blockchain messages
  - Purpose: Cross-chain message authentication and validation
  - Implementation:
    - `Chainweb.VerifierPlugin.Hyperlane.Message` (`src/Chainweb/VerifierPlugin/Hyperlane/Message.hs`)
    - Version-specific handlers: `Before225` and `After225` for protocol versions
    - Announcement parsing and binary message handling
  - Usage: Custom verifier plugin for Pact transactions on Hyperlane-enabled chains

**SignedList Verification Plugin:**
- Purpose: Validate transactions against cryptographically signed validator lists
- Implementation: `Chainweb.VerifierPlugin.SignedList` (`src/Chainweb/VerifierPlugin/SignedList.hs`)
- Usage: Custom transaction verification in Pact

## Data Storage

**Databases:**
- RocksDB (Primary)
  - Provider: Kadena-optimized fork at `https://github.com/kadena-io/rocksdb-haskell.git`
  - Package: `rocksdb-haskell-kadena 1.1.0+`
  - Connection: In-process C library via FFI
  - Client: `Chainweb.Storage.Table.RocksDB` from `chainweb-storage` library
  - Purpose: Block headers, cut database, payload storage, peer database
  - Location: Default `_chainweb` directory (configurable)

- SQLite3 (Secondary)
  - Package: `direct-sqlite 2.3.27+` (raw FFI bindings)
  - Connection: Direct C FFI, embedded in binary
  - Client: `Chainweb.Pact.Backend.SQLite.V2` (`src/Chainweb/Pact/Backend/SQLite/V2.hs`)
  - Purpose: Pact smart contract state, key-value store for contract tables
  - Special: Custom SHA3 functions compiled via `c/shathree.c`
  - Location: Per-chain database files in pact state directory

**Storage Abstraction Layer:**
- `chainweb-storage 0.1+` library (`libs/chainweb-storage/`)
  - Pluggable backends: RocksDB, HashMap (in-memory), Forgetful (write-only)
  - Used by: `Chainweb.Payload.PayloadStore.RocksDB`, block header storage, peer database
  - Location: `libs/chainweb-storage/src/` and `chainweb-storage.cabal`

**File Storage:**
- Local filesystem only
- Configuration stored in YAML files
- Pact genesis blocks and contract code in `pact/genesis/` and `pact/coin-contract/`
- CSV data in `allocations/` (Mainnet/Testnet keysets) and `rewards/` (miner rewards)

**Caching:**
- In-memory caching via Haskell data structures
  - Module cache: `Chainweb.Pact4.ModuleCache` and `Chainweb.Pact5.Backend.ChainwebPactDb`
  - Block header cache: `Chainweb.BlockHeaderDB.HeaderOracle`
- No external cache service (Redis, Memcached)
- STM-based concurrent caches for thread-safe access

## Authentication & Identity

**Auth Provider:**
- Custom implementation (no external OAuth/OIDC)
- Digital signature verification using cryptographic keys
- Approach:
  - Transaction signing via Ed25519 or ECDSA (configurable)
  - Public key validation in `Chainweb.Pact4.Validations` and `Chainweb.Pact5.Validations`
  - Signature verification in transaction execution
  - Optional ED25519 certificates via `ed25519` cabal flag (TLS support)
- X.509 certificate support for node-to-node TLS connections
  - Self-signed certificate generation: `Network.X509.SelfSigned` (`src/Network/X509/SelfSigned.hs`)
  - Certificate validation: `crypton-x509-validation` library

## Monitoring & Observability

**Error Tracking:**
- None (no external error tracking service like Sentry, Rollbar)
- Local error handling via Haskell exceptions and custom error types

**Logging:**
- Yet-Another-Logger (YAL) 0.4.1+
  - Configuration: `Utils.Logging.Config` (`src/Utils/Logging/Config.hs`)
  - Structured logging with JSON output support
  - Async logging for performance
  - Log level control via `LogLevel` package
  - Trace logging support: `Utils.Logging.Trace`
  - Miner-specific logging: `Chainweb.Logging.Miner`
  - Request logging: `Chainweb.Utils.RequestLog` (`src/Chainweb/Utils/RequestLog.hs`)

**Metrics & Monitoring:**
- Custom metrics via in-process counters
  - `Chainweb.Counter` module for performance metrics
  - Stopwatch: `stopwatch 0.1+` library for timing operations
  - Network timing: `Chainweb.Utils.RequestLog`
- No external metrics collector (Prometheus, Datadog, etc.)

## CI/CD & Deployment

**Hosting:**
- Supports any Linux x86_64 or aarch64 platform
- Docker containerization: `Dockerfile` with multi-stage builds
  - Base: Ubuntu 22.04
  - GHC 9.10.1 toolchain
  - Targets: `chainweb-node` (minimal), `chainweb-applications` (all tools)
- Cloud deployment: Manual or CI pipeline integration (no built-in cloud provider config)

**CI Pipeline:**
- GitHub Actions (configuration in `.github/workflows/` - not analyzed)
- Nix Flakes for reproducible builds across platforms
- Docker buildkit support for cross-platform images
- Pre-commit checks: GHC compiler warnings treated as errors

## Environment Configuration

**Required Environment Variables:**
- None hard-coded or enforced by default
- Configuration via YAML files (see `.envrc.recommended` for Nix)
- Key runtime options (TLS cert paths, port, peer addresses) configurable via CLI

**Secrets Location:**
- Private keys: External configuration (TLS cert keys, node signing keys)
  - Stored as PEM files (configured at runtime)
  - No built-in secrets vault (use OS-level key management)
- Database credentials: Not applicable (local storage)
- API tokens: Not used (custom signature-based auth)

## Webhooks & Callbacks

**Incoming:**
- None (no external webhooks consumed)

**Outgoing:**
- P2P block broadcast via custom Chainweb protocol
  - Nodes push newly mined blocks to peers
  - Mempool synchronization via `Chainweb.Chainweb.MempoolSyncClient`
  - Implementation: `P2P.Node.RestAPI.Server` and `P2P.Node.RestAPI.Client`

## Network Communication

**P2P Protocol:**
- Custom binary protocol (not HTTP)
- Peer discovery and bootstrap
  - Bootstrap nodes: `P2P.BootstrapNodes` (`src/P2P/BootstrapNodes.hs`)
  - Peer database: `P2P.Node.PeerDB` (`src/P2P/Node/PeerDB.hs`)
- Reachability checks: `Chainweb.Chainweb.CheckReachability`
- Block synchronization: `Chainweb.Sync.WebBlockHeaderStore` (`src/Chainweb/Sync/WebBlockHeaderStore.hs`)

**REST API Endpoints:**
- BlockHeader API: `Chainweb.BlockHeaderDB.RestAPI`
  - Endpoints for querying block headers and mining info
  - Streaming support via Server-Sent Events
- CutDB API: `Chainweb.CutDB.RestAPI`
  - Cross-chain consensus state queries
- Mempool API: `Chainweb.Mempool.RestAPI`
  - Transaction submission and status
- Mining API: `Chainweb.Miner.RestAPI`
  - Work request and submission for proof-of-work miners
- Pact API: `Chainweb.Pact.RestAPI.Server` (`src/Chainweb/Pact/RestAPI/Server.hs`)
  - Smart contract queries and transaction submission
  - Ethereum SPV verification endpoint: `EthSpv` POST handler
  - SPV proof generation: `Chainweb.Pact.RestAPI.SPV`
- Payload API: `Chainweb.Payload.RestAPI`
  - Transaction list and proof queries
- SPV API: `Chainweb.SPV.RestAPI`
  - Simple Payment Verification proof endpoints
  - Event, output, and payload proof generation

**API Clients:**
- Generated via Servant from type-safe API definitions
- Auto-generated client functions in RestAPI.Client modules
- HTTP client: `http-client 0.5+` with TLS via `http-client-tls`
- Connection pooling handled by http-client

## Transaction Processing

**Mempool:**
- In-process transaction pool
- Backend: Pluggable (in-memory via `Chainweb.Mempool.InMem`)
- Consensus rules: `Chainweb.Mempool.Consensus`
- Sync across network: `Chainweb.Mempool.P2pConfig` and `Chainweb.Chainweb.MempoolSyncClient`

**Smart Contract Execution:**
- Pact 4 execution: `Chainweb.Pact4.TransactionExec.hs`
- Pact 5 execution: `Chainweb.Pact5.TransactionExec.hs`
- Database backend: SQLite with RocksDB overlay for performance
- Checkpointing: `Chainweb.Pact.PactService.Checkpointer` for state durability
- Block validation: `Chainweb.Pact.Service.BlockValidation`

---

*Integration audit: 2026-02-10*

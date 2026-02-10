# Technology Stack

**Analysis Date:** 2026-02-10

## Languages

**Primary:**
- Haskell 2010 - Core blockchain implementation in `src/`, all library packages
- C - Cryptographic utilities in `c/shathree.c` (SHA3 implementation) and `c/rlim_utils.c` (resource limit utilities)

**Secondary:**
- Pact - Smart contract language (v4 and v5) in `pact/coin-contract/`
- YAML - Configuration and test data (`pact/genesis/*.yaml`, `allocations/`)
- Shell/Bash - Build and deployment scripts

## Runtime

**Environment:**
- GHC (Glasgow Haskell Compiler) 9.10 (primary) or 9.8 (fallback)
- Cabal 3.4+ (package manager and build tool)
- Alternative: Nix (`nix build` / `nix develop`)

**Package Manager:**
- Cabal (Haskell) 3.8
- Lockfile: implicit in `cabal.project` and Nix flake

**Platform Support:**
- Linux: x86_64, aarch64 (ubuntu:22.04 in Dockerfile)
- macOS: aarch64, x86_64 (requires `/opt/homebrew/` for libraries, configured in `cabal.project`)

## Frameworks

**Core Blockchain:**
- Pact 4.2.0.1+ - Smart contract execution engine (Kadena fork from `https://github.com/kadena-io/pact.git`)
- Pact 5.0+ - Next-generation smart contract engine (`pact-5.git` from Kadena)
- Pact-TNG - Transaction handling and request API (`pact-tng` and `pact-tng:pact-request-api`)
- Pact-JSON 0.1+ - JSON serialization for Pact values

**Web & REST API:**
- Servant 0.20.1+ - Type-safe REST API framework (`servant`, `servant-server`, `servant-client`)
- Warp 3.3.6+ - HTTP server (`warp`, `warp-tls` for TLS support)
- Wai 3.2.2.1+ - Web Application Interface middleware and utilities
- Wai-CORS 0.2.7+ - CORS middleware
- Wai-Extra 3.0.28+ - Additional WAI middleware
- Wai-Middleware-Throttle 0.3+ - Request rate limiting
- Wai-Middleware-Validation - Custom request validation middleware (Kadena fork)

**Data Serialization:**
- Aeson 2.2+ - JSON encoding/decoding
- YAML 0.11+ - YAML parsing for configuration and test data
- Cassava 0.5.1+ - CSV parsing for allocations and rewards data

**Database & Storage:**
- RocksDB (via `rocksdb-haskell-kadena` 1.1.0+) - Primary persistent key-value storage backend
- SQLite3 (via `direct-sqlite` 2.3.27+) - Pact state storage (`Chainweb.Pact.Backend.SQLite`)
- Chainweb-Storage 0.1+ - Abstract pluggable storage layer with RocksDB, HashMap, and Forgetful backends

**Cryptography:**
- Crypton 0.31+ - Modern cryptographic primitives (replaces cryptonite)
- Crypton-X509 1.7+ - X.509 certificate handling
- Crypton-X509-Validation 1.6+ - X.509 validation
- Crypton-Connection 0.4.2+ - TLS connection support
- TLS 2.1.4+ - TLS protocol implementation
- TLS-Session-Manager 0.0+ - Session management for TLS
- PEM 0.2+ - PEM encoding/decoding for certificates
- Memory 0.14+ - Memory allocation utilities
- Ethereum SDK (`ethereum:{ethereum, secp256k1}` 0.1+) - Ethereum cryptography and types for SPV

**Concurrency & Async:**
- Async 2.2+ - Asynchronous I/O utilities
- STM 2.4+ - Software transactional memory for concurrent state
- Monad-Control 1.0+ - Monad control operations
- Unliftio 0.2+ - Lifted I/O utilities
- Safe-Exceptions 0.1+ - Exception handling
- Exceptions 0.8+ - Exception infrastructure

**Data Structures & Algorithms:**
- Vector 0.12.2+ - Efficient arrays
- Vector-Algorithms 0.7+ - Sorting and searching
- Containers 0.5+ - Standard collections (Map, Set, etc.)
- Unordered-Containers 0.2.20+ - Hash tables and sets
- Lens 4.17+ - Composable record accessors and optics
- Heaps 0.3+ - Priority queue data structures
- Cuckoo 0.3+ - Cuckoo hashing
- IxSet-Typed 0.4+ - Type-safe indexed sets (Kadena fork)

**Utility Libraries:**
- Optparse-Applicative 0.14+ - Command-line argument parsing
- Text 2.0+ - Unicode text handling
- ByteString - Efficient byte sequences
- Time 1.12.2+ - Time and date handling
- Clock 0.7+ - High-resolution timing
- Chronos 1.1+ - Time library
- Random 1.3+ - Random number generation
- MWC-Random 0.13+ - Mersenne Twister PRNG
- MWC-Probability 2.0+ - Probability distribution sampling
- Configuration-Tools 0.6+ - Configuration file parsing
- Managed 1.0+ - Resource management
- Resourcet 1.3+ - Resource allocation and cleanup

**Logging & Observability:**
- Yet-Another-Logger 0.4.1+ - Structured logging framework
- LogLevel 0.1+ - Log level management

**Testing:**
- Tasty 1.0+ - Test framework and runner
- Tasty-QuickCheck 0.9+ - QuickCheck integration
- Tasty-HUnit 0.9+ - Unit testing support
- Tasty-Golden 2.3+ - Golden file testing
- QuickCheck 2.14+ - Property-based testing
- QuickCheck-Instances 0.3+ - Standard type generators
- Property-Matchers 0.7+ - Property matching utilities
- Temporary 1.3+ - Temporary file/directory creation

**Build & Development:**
- Template-Haskell 2.14+ - Compile-time metaprogramming
- File-Embed 0.0+ - Compile-time file embedding
- Hsinspect - GHC flags plugin (optional via `ghc-flags` flag)

**Network & Crypto Utilities:**
- Network 3.1.2+ - Socket and networking
- HTTP-Client 0.5+ - HTTP client library
- HTTP-Client-TLS 0.3+ - TLS support for HTTP client
- HTTP2 5.2.1+ - HTTP/2 protocol support
- HTTP-Types 0.12+ - HTTP request/response types
- HTTP-Media 0.7+ - Media type handling
- IProute 1.7+ - IP routing data types
- Base16-ByteString 0.1+ - Base16 encoding/decoding

## Key Dependencies

**Critical Infrastructure:**
- `rocksdb-haskell-kadena 1.1.0+` - Block and state data persistence; optimized Kadena fork
- `pact 4.2.0.1+` and `pact-tng` - Smart contract VM and transaction execution
- `chainweb-storage 0.1+` - Abstract storage backend with pluggable implementations
- `servant 0.20.1+` - Type-safe REST API definition and generation

**Blockchain Core:**
- `ethereum:{ethereum, secp256k1}` - Ethereum SPV verification and cryptography for bridge
- `wai-middleware-validation` - Custom request validation (Kadena fork for validation rules)
- `base64-bytestring-kadena 0.1` - Base64 with non-canonical decode support (Kadena fork)

**Consensus & Sync:**
- `merkle-log 0.2+` - Merkle tree computation for block verification
- `binary 0.8+` - Binary serialization
- `patience 0.3+` - Patience diff algorithm for block synchronization

**Advanced Utilities:**
- `lens 4.17+` - Functional record field access and composition
- `mmorph 1.1+` - Monad morphism abstractions
- `semigroupoids 5.3.7+` - Categorical operations
- `semialign 1.3.1+` - Alignment operations for data structures
- `these 1.0+` - Either-like type for both/either/these cases

## Configuration

**Environment Configuration:**
- Nix Flakes (`flake.nix`) - Reproducible development environment with Haskell.nix
- Cabal project file (`cabal.project`) - Platform-specific build settings
- `.envrc.recommended` - Direnv integration for automatic Nix shell entry
- Docker multi-stage builds (`Dockerfile`) with Ubuntu 22.04 base

**macOS-Specific:**
- OpenSSL at `/opt/homebrew/opt/openssl/` (aarch64) or `/opt/local/` (x86_64)
- Library paths configured in `cabal.project` for both architectures

**Build Configuration:**
- Custom Setup.hs for `chainweb-node` with source file generation and Git metadata
- GHC compiler flags enforced: `-Wall -Werror -Wcompat` (all warnings treated as errors)
- Language extensions: `ImportQualifiedPost`, `OverloadedStrings`, `DataKinds`, `TypeFamilies`, `ScopedTypeVariables`
- Optimization: `-O2` with `ghc-compact` memory optimizations for runtime efficiency

**Test Configuration:**
- Separate test suites:
  - `chainweb-tests` (unit tests, co-located with source)
  - `compaction-tests` (database compaction validation)
  - `multi-node-network-tests` (P2P network behavior)
  - `remote-tests` (distributed database operations)

## Platform Requirements

**Development:**
- GHC 9.8 or 9.10
- Cabal 3.4+
- OpenSSL development headers
- For macOS: Homebrew with openssl@1.1 or openssl@3
- For Linux: Standard build-essentials + openssl-dev
- Optional: Nix (for reproducible builds)
- Optional: Docker/Buildkit (for containerized builds)

**Production:**
- Deployment target: Linux x86_64 or aarch64 (Ubuntu 22.04 compatible)
- Containerized via Docker using multi-stage builds
- Resource requirements: 1GB+ heap (`-H1G` in RTS), 64MB allocation area (`-A64M`)
- Network: P2P connectivity, TLS support (port configurable, typically 443)

---

*Stack analysis: 2026-02-10*

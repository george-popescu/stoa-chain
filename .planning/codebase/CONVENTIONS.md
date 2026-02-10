# Coding Conventions

**Analysis Date:** 2026-02-10

## Language Extensions

Standard extensions enabled throughout the codebase. Core set enforced in `chainweb.cabal`:
- `ImportQualifiedPost` - Use qualified imports with post-fix syntax: `import Foo qualified as F`
- `OverloadedStrings` - String literals work as Text
- `DataKinds` - Lift types to kind level (common for type-level programming)
- `TypeFamilies` - Associated type families
- `ScopedTypeVariables` - Explicit type variables in expressions
- `DeriveAnyClass` - Derive via typeclass constraints
- `DerivingStrategies` - Explicit deriving strategies
- `GeneralizedNewtypeDeriving` - Newtypes derive instances automatically
- `FlexibleInstances` - Relax instance head restrictions
- `FlexibleContexts` - Relax context restrictions
- `GADTs` - Generalized algebraic data types
- `TypeApplications` - Apply types explicitly with `@Type` syntax
- `TemplateHaskell` - Metaprogramming support (used for lenses)
- `RankNTypes` - Higher-rank polymorphism
- `MultiParamTypeClasses` - Multiple parameter typeclasses
- `LambdaCase` - Pattern match on function argument

## Naming Patterns

**Modules:**
- Public API modules exposed directly: `Chainweb.BlockHeader`, `Chainweb.BlockHash`
- Internal implementation modules in `.Internal` submodules: `Chainweb.BlockHeader.Internal`
  - Location: `src/Chainweb/BlockHeader/Internal.hs`
  - Purpose: Houses dangerous operations (manual construction, writable setters)
- REST API modules in `.RestAPI` submodules: `Chainweb.BlockHeaderDB.RestAPI`
  - Contains both server and client API definitions
  - Separated into `.RestAPI.Server` and `.RestAPI.Client` when large
- Test modules mirror source hierarchy: `Chainweb.Test.BlockHeader`, `Chainweb.Test.SPV.EventProof`
  - Location: `test/unit/Chainweb/Test/`
- Test utility modules in `test/lib/`: `Chainweb.Test.Utils`, `Chainweb.Test.Orphans.Internal`

**Functions:**
- camelCase for all functions: `blockCreationTime`, `encodeBlockHash`, `getAdjacentHash`
- Getter functions (via lens): `blockCreationTime :: Getter I.BlockHeader BlockCreationTime`
- Encoding functions: `encodeBlockHeader`, `encodeBlockHash`
- Decoding functions: `decodeBlockHeader`, `decodeBlockHash`
- Decoding with validation: `decodeBlockHeaderChecked`, `decodeBlockHashRecordChecked`
- Constructor functions: `newBlockHeader`, `genesisBlockHeader`
- Predicate functions: `isGenesisBlockHeader`, `isForkEpochStart`
- Unsafe/internal variants prefixed with underscore: `_blockPow`, `_rankedBlockHash`, `_blockAdjacentChainIds`

**Types:**
- PascalCase for all types: `BlockHeader`, `BlockHash`, `ChainId`, `BlockHeight`
- Newtype wrappers: `ParentHeader`, `ParentCreationTime`, `Nonce`
- Exception types end in standard suffix: `DecodeException`, `TextFormatException`, `X509CertificateDecodeException`
- Type families and GADT constructors: `ObjectEncoded`, `ExtendedObjectEncoded`

**Records and Fields:**
- Use qualified post-fix imports to avoid namespace pollution
- Fields accessed via lenses/getters, not direct record access for public APIs
- Internal records use prefix pattern: `BlockHeader_(..)`
  - Example: `BlockPayloadHash_(..)`

**Constants and Values:**
- camelCase for values: `coinReplV1`, `toyVersion`
- Underscore prefix for internal/implementation values: `_rankedBlockHash`

## Import Organization

**Order of imports:**

1. Language pragmas and {-# OPTIONS_GHC #-} directives at top
2. Module documentation (Haddock comment)
3. Module declaration with qualified imports
4. Explicit imports from other modules
5. Qualified imports (with `qualified as` suffix)
6. System/Platform imports
7. Internal imports (chainweb or project-specific)

**Example from `src/Chainweb/BlockHash.hs`:**
```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
-- ... more pragmas

-- |
-- Module: Chainweb.BlockHash
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb block hashes...

module Chainweb.BlockHash
( BlockHash
, BlockHash_(..)
-- exports ...
) where

import Control.DeepSeq
import Control.Lens
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)

import Data.Aeson
    (FromJSON(..), FromJSONKey(..), ToJSON(..), ToJSONKey(..), withText)
import Data.Aeson.Types (FromJSONKeyFunction(..), toJSONKeyText)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics hiding (to)

import Numeric.Natural

-- internal imports
import Chainweb.BlockHeight
import Chainweb.ChainId
import Chainweb.Crypto.MerkleLog
import Chainweb.Graph
```

**Path Aliases:**
- No path aliases defined in project. All imports use full module paths.

## Code Style

**Compiler Flags:**

All packages enforce strict warning flags in `chainweb.cabal`:
```
ghc-options:
    -Wall
    -Werror
    -Wcompat
    -Wpartial-fields
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Widentities
    -funclutter-valid-hole-fits
    -fmax-relevant-binds=0
```

**Formatting:**
- No explicit formatter enforced (no .prettierrc or similar)
- Convention appears to be 4-space indentation
- Line width not strictly enforced
- Brace placement: opening brace on same line (standard Haskell style)

**Linting:**
- Compiler warnings are errors (`-Werror`)
- No external linter (no HLint config)
- Code must compile cleanly with `-Wall -Wcompat`

## Haddock Documentation

**Module-level documentation:**
```haskell
-- |
-- Module: Chainweb.BlockHash
-- Copyright: Copyright © 2018 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Chainweb block hashes. A block hash identifies a node in the Merkle tree...
```

**Function documentation:**
- Inline Haddock (`-- |`) precedes functions
- Describe purpose and behavior
- Example from `src/Chainweb/BlockHash.hs`:
```haskell
-- | Encode a BlockHash to binary
encodeBlockHash :: (Serialize a) => BlockHash a -> ByteString
```

**Export list organization:**
- Group related exports with comments
- Example from `src/Chainweb/BlockHash.hs`:
```haskell
module Chainweb.BlockHash
( -- * BlockHash
  BlockHash
, BlockHash_(..)
, encodeBlockHash
, decodeBlockHash
, nullBlockHash
-- * Block Hash Record
, BlockHashRecord(..)
, getBlockHashRecord
-- * Exceptions
)
```

**Documentation style:**
- Use `-- |` for Haddock-documented items
- Use `-- --` separator lines for section breaks: `-- -------------------------------------------------------------------------- --`
- Use `-- *` for top-level section headers in exports
- Use `-- **` for subsection headers in exports

## Error Handling

**Exception Types:**
- Custom exceptions defined in modules and exported from module exports
- Hierarchy examples:
  - `DecodeException` - Generic decode failures
  - `TextFormatException` - Text format parsing issues
  - `X509CertificateDecodeException` - X509 certificate decode failure
  - `IntegerOutOfBoundsException` - Integer outside allowed bounds

**Pattern: MonadThrow:**
```haskell
import Control.Monad.Catch (MonadThrow, throwM)

blockHashFromText :: MonadThrow m => T.Text -> m BlockHash
blockHashFromText t = either (throwM . TextFormatException . sshow) return
    $ decodeBlockHash t
```

**Pattern: Either for validation:**
```haskell
decodeBlockHeader :: ByteString -> Either String BlockHeader
decodeBlockHeaderChecked :: ChainId -> ByteString -> Either String BlockHeader
```

**Pattern: Try/Catch with exceptions:**
```haskell
try :: Exception e => IO a -> IO (Either e a)
fromException :: Exception e => SomeException -> Maybe e
```

Used in test code for property-based testing of exceptions:
```haskell
prop_int256_outOfBounds_maxBound :: Property
prop_int256_outOfBounds_maxBound =
    first fromException (int256 x) === Left (Just (IntegerOutOfBoundsException x))
```

## Comments and TODOs

**In-code comments:**
- Use `--` for single-line comments
- Use `{- ... -}` for block comments
- Haddock style (`-- |`, `-- *`) for public API documentation

**TODO/FIXME patterns found:**
```haskell
-- TODO(greg): BlockHashRecord should be a sorted vector
-- FIXME: add proper error handling
-- TODO: use the transaction tree cache
```

Location convention: Place directly above or on same line as relevant code.

## Function Design

**Parameter organization:**
- Type parameters come first
- Constraint parameters in parentheses: `(MonadThrow m) =>`
- Effect/monad parameter comes before data parameters
- Example: `arbitraryEventsProof :: forall a. MerkleHashAlgorithm a => Property`

**Return values:**
- Use `Maybe a` for optional values
- Use `Either String a` or `Either SomeException a` for fallible operations
- Use `IO a` for side effects
- Use `Property` for QuickCheck properties

**Higher-order functions:**
- Common pattern: fold-like functions over structures
- Lenses used extensively for getters/setters

## Module Design

**Public API vs Internal:**
- Public modules re-export limited interface from `.Internal`
- Example `src/Chainweb/BlockHeader.hs` exports getters only:
```haskell
blockFlags :: Getter I.BlockHeader ForkState
blockFlags = I.blockFlags

blockCreationTime :: Getter I.BlockHeader BlockCreationTime
blockCreationTime = I.blockCreationTime
```

**Orphan instances:**
- Allowed with `{-# OPTIONS_GHC -fno-warn-orphans #-}`
- Collected in `Chainweb.Test.Orphans.Internal` for test instances
- Examples: `Arbitrary ChainwebVersion`, `Arbitrary BlockHash`

**Barrel files:**
- Not used; each module provides focused exports
- Test utilities grouped in `test/lib/` but each module independent

## Pact Integration Conventions

**Multiple Pact versions supported:**
- `Chainweb.Pact4.*` - Pact version 4 integration
- `Chainweb.Pact5.*` - Pact version 5 integration
- Original `Chainweb.Pact.*` - Legacy/v3

**Template functions:**
- `Chainweb.Pact4.Templates` - Transaction template builders
- `Chainweb.Pact5.*` - Version-specific implementations

---

*Convention analysis: 2026-02-10*

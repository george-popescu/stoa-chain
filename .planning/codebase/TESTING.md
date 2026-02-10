# Testing Patterns

**Analysis Date:** 2026-02-10

## Test Framework

**Runner:**
- Tasty 1.x (test suite framework)
- QuickCheck (property-based testing)
- HUnit (unit test assertions)
- Config: `chainweb.cabal` (test suite configuration)

**Assertion Library:**
- Test.Tasty.HUnit for imperative assertions
- Test.QuickCheck for properties
- Custom assertion helpers in `Chainweb.Test.Utils`

**Run Commands:**
```bash
# From stoa-chain/ directory
cabal test                           # Run all test suites
cabal test chainweb-tests            # Main test suite
cabal test compaction-tests          # Database compaction tests
cabal test multi-node-network-tests  # P2P network tests
cabal test remote-tests              # Remote database tests
```

**Test Suites Defined in `chainweb.cabal`:**
1. `chainweb-tests` - Main unit test suite (fast, local, parallel)
2. `compaction-tests` - Database compaction tests
3. `multi-node-network-tests` - Multi-node P2P tests
4. `remote-tests` - Remote database tests

## Test File Organization

**Location Pattern:**
- Co-located with source: Mirror of `src/` exists in `test/unit/`, `test/lib/`, `test/pact/`
- Directory structure: `test/unit/Chainweb/Test/` mirrors `src/Chainweb/`

**File Naming:**
```
src/Chainweb/BlockHeader.hs          → test/unit/Chainweb/Test/BlockHeader.hs
src/Chainweb/SPV/EventProof.hs       → test/unit/Chainweb/Test/SPV/EventProof.hs
src/Chainweb/BlockHash.hs            → (no dedicated test file, tests in Roundtrips.hs)
```

**Directory Structure:**
```
test/
├── unit/               # Fast unit tests that must:
│   ├── ChainwebTests.hs      # Main entry point
│   └── Chainweb/Test/        # Mirror of src/Chainweb/
│       ├── BlockHeader/
│       ├── SPV/
│       ├── Pact4/            # Pact v4 tests
│       ├── Pact5/            # Pact v5 tests
│       └── Roundtrips.hs      # Encode/decode roundtrip tests
├── lib/                # Shared test utilities (no assertions)
│   ├── Chainweb/Test/
│   │   ├── Utils.hs           # Fixture builders, test resources
│   │   ├── Orphans/           # Arbitrary instances for QuickCheck
│   │   └── Pact4/Utils.hs
│   └── Servant/Client_.hs
├── pact/               # Pact smart contract test files (.pact, .repl)
├── compaction/         # Database compaction tests
├── multinode/          # Multi-node P2P tests
└── remote/             # Remote database tests
```

## Test Structure

**Module exports pattern:**
Every test module exports a top-level `tests :: TestTree` or `tests :: RocksDb -> TestTree`:

```haskell
-- test/unit/Chainweb/Test/SPV/EventProof.hs
module Chainweb.Test.SPV.EventProof
( tests
, properties
, testCases
, PactEventDecimal(..)
, PactEventModRef(..)
) where

tests :: TestTree
tests = testGroup "Chainweb.Test.SPV.EventProof"
    [ properties
    , testCases
    ]
```

**Test group organization:**
```haskell
tests :: TestTree
tests = testGroup "Chainweb.Test.Pact4.TransactionTests"
  [ testGroup "Pact Command Parsing"
    [ testCase "Build Exec with Data" buildExecWithData
    , testCase "Build Exec without Data" buildExecWithoutData
    ]
  , testGroup "Pact Code Unit Tests"
    [ testGroup "Coin Contract repl tests"
      [ testCase "v1" (ccReplTests coinReplV1)
      , testCase "v4" (ccReplTests coinReplV4)
      ]
    ]
  ]
```

**Entry point:**
```haskell
-- test/unit/ChainwebTests.hs
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Chainweb"
    [ Chainweb.Test.BlockHeader.Validation.tests
    , Chainweb.Test.SPV.EventProof.tests
    , Chainweb.Test.Roundtrips.tests
    -- ... more test modules
    ]
```

## Test Guidelines (from comments)

Tests in `chainweb-tests` must:
- Execute quickly
- NOT depend on remote resources
- Run in parallel with other tests
- Be focused on a well-defined function or feature

Guidelines for database tests:
- Avoid initializing a new RocksDb
- Create temporary RocksDB as overlay of provided RocksDb resource
- Use `withTestRocksDb` or similar resource helpers

## Property-Based Testing with QuickCheck

**Test declaration:**
```haskell
[ testProperty "int256_maxBound" prop_int256_maxBound
, testProperty "int256" prop_int256
, testProperty "decimal" prop_decimal
]
```

**Property patterns:**

1. **Simple boolean property:**
```haskell
prop_merkleProof_run :: MerkleHashAlgorithm a => MerkleProof a -> Bool
prop_merkleProof_run p = case runMerkleProof p of !_ -> True
```

2. **Property with forAll generator:**
```haskell
prop_eventsProof_run
    :: forall a
    . MerkleHashAlgorithm a
    => Property
prop_eventsProof_run = forAll (arbitraryEventsProof @a) $ \p ->
    case runMerkleProof (_payloadProofBlob p) of !_ -> True
```

3. **Roundtrip property (encode/decode):**
```haskell
prop_supportedDecimal :: Decimal -> Property
prop_supportedDecimal d
    | isSupportedDecimal d = roundtripped === Right d
    | otherwise = roundtripped === Left (DecimalOutOfBoundsException d)
  where
    roundtripped = case try $ integerToDecimal <$> decimalToInteger d of
        Left e -> error (show e)
        Right x -> x
```

4. **Exception testing:**
```haskell
prop_int256_outOfBounds_maxBound :: Property
prop_int256_outOfBounds_maxBound =
    first fromException (int256 x) === Left (Just (IntegerOutOfBoundsException x))
  where
    x = int256ToInteger maxBound + 1
```

5. **Use `once` for expensive properties:**
```haskell
prop_int256_maxBound :: Property
prop_int256_maxBound = once
    $ unsafeInt256 (2^(255 :: Int) - 1) === maxBound
```

6. **Use `counterexample` for debugging:**
```haskell
fail_eventsProof_run2 = forAll (arbitraryEventsProof @a) $ \p ->
    case runEventsProof p of
        Left e -> counterexample ("failed to validate proof: " <> show e) False
        Right (!_, !_) -> property True
```

## Custom Test Instances (Arbitrary)

**Location:** `test/lib/Chainweb/Test/Orphans/Internal.hs`

**Pattern for Arbitrary instances:**
```haskell
instance Arbitrary ChainwebVersion where
    arbitrary = elements
        [ barebonesTestVersion singletonChainGraph
        , barebonesTestVersion petersenChainGraph
        , timedConsensusVersion singletonChainGraph singletonChainGraph
        , timedConsensusVersion petersenChainGraph petersenChainGraph
        , RecapDevelopment
        , Testnet04
        , Mainnet01
        ]

newtype PactEventDecimal = PactEventDecimal { _getPactEventDecimal :: Decimal }
    deriving (Show, Eq, Ord)

instance Arbitrary PactEventDecimal where
    arbitrary = fmap PactEventDecimal $ Decimal
        <$> choose (0, 18)
        <*> arbitrary
```

**Generator helpers:**
- `arbitraryBytes :: Int -> Gen B.ByteString` - Fixed-size random bytes
- `arbitraryBytesSized :: Gen B.ByteString` - Size-based random bytes
- `arbitraryBlockHeaderVersion` - Valid block headers
- `arbitraryEventsProof` - Valid event proofs with merkle trees
- `arbitraryOutputProof` - Valid output proofs

**Handling orphan instances:**
- File marked with `{-# OPTIONS_GHC -fno-warn-orphans #-}`
- Instances for external types (Pact, Merkle) go here
- Separated from main library code

## Unit Tests with HUnit

**Declaration pattern:**
```haskell
testCase :: String -> Assertion -> TestTree

testCase "Build Exec with Data" buildExecWithData
testCase "v1" (ccReplTests coinReplV1)
```

**Assertion patterns:**

1. **Basic equality:**
```haskell
buildExecWithData :: Assertion
buildExecWithData = do
    result <- parseCommand cmd
    result @?= expected
```

2. **IO effects with assertions:**
```haskell
ccReplTests :: FilePath -> Assertion
ccReplTests ccFile = do
    (r, rst) <- execScript' Quiet ccFile
    either fail (\_ -> execRepl rst) r
```

3. **Custom expectations:**
```haskell
assertExpectation :: String -> IO a -> (a -> Bool) -> Assertion
assertExpectation msg action predicate = do
    result <- action
    unless (predicate result) $ fail msg
```

4. **Golden file comparison:**
```haskell
golden :: FilePath -> IO String -> Assertion
-- Compares output against golden file
```

## Integration Tests

**Database resource management:**
```haskell
-- From test/lib/Chainweb/Test/Utils.hs
testRocksDb :: IO RocksDb
withTestRocksDb :: (RocksDb -> IO ()) -> IO ()

testBlockHeaderDb :: IO BlockHeaderDb
withTestBlockHeaderDb :: (BlockHeaderDb -> IO ()) -> IO ()
```

**SQLite resource helpers:**
```haskell
withTempSQLiteResource :: (SqliteDb -> IO ()) -> IO ()
withInMemSQLiteResource :: (SqliteDb -> IO ()) -> IO ()
```

**Multi-node testing:**
```haskell
withNodesAtLatestBehavior :: [ConfigModifier] -> (ChainwebNetwork -> IO ()) -> IO ()
withNodes :: [ConfigModifier] -> (ChainwebNetwork -> IO ()) -> IO ()
awaitBlockHeight :: ChainId -> BlockHeight -> ChainwebNetwork -> IO ()
```

## Test Data and Fixtures

**Builder patterns in `test/lib/Chainweb/Test/Utils.hs`:**
```haskell
-- Generate toy/test data
toyVersion :: ChainwebVersion
toyChainId :: ChainId
toyGenesis :: BlockHeader

-- Create test fixtures
genesisBlockHeaderForChain :: ChainId -> ChainwebVersion -> BlockHeader
toyBlockHeaderDb :: IO BlockHeaderDb
withToyDB :: (BlockHeaderDb -> IO ()) -> IO ()

-- Tree operations for block header tests
prettyTree :: BlockHeaderDbs -> IO String
normalizeTree :: Tree -> Tree
treeLeaves :: Tree -> [BlockHash]

-- Insert test data
insertN :: BlockHeaderDb -> Int -> IO ()
insertN_ :: BlockHeaderDb -> Int -> IO ()
```

**Pact-specific fixtures:**
```haskell
-- From test/lib/Chainweb/Test/Pact4/Utils.hs and Pact5/Utils.hs
coinReplV1 :: FilePath
coinReplV4 :: FilePath
coinReplV5 :: FilePath
coinReplV6 :: FilePath

logger :: GenericLogger
-- Configured for test mode (Info or Error level based on DEBUG_TEST flag)
```

## Roundtrip and Encoding Tests

**Pattern in `test/unit/Chainweb/Test/Roundtrips.hs`:**
```haskell
tests :: TestTree
tests = testGroup "roundtrip tests"
    [ encodeDecodeTests
    , showReadTests
    , base64RoundtripTests
    , hasTextRepresentationTests
    , jsonRoundtripTests
    , pactJsonRoundtripTests
    , jsonKeyRoundtripTests
    , timeSpanTests
    ]

encodeDecodeTests :: TestTree
encodeDecodeTests = testGroup "Encode-Decode roundtrips"
    [ testProperty "ChainwebVersionCode"
        $ prop_encodeDecode decodeChainwebVersionCode encodeChainwebVersionCode
    , testProperty "ChainId"
        $ prop_encodeDecode decodeChainId encodeChainId
    , testProperty "BlockHash"
        $ prop_encodeDecode decodeBlockHash (encodeBlockHash @ChainwebMerkleHashAlgorithm)
    ]
```

**Helper property:**
```haskell
prop_encodeDecode :: (Eq a, Show a) => (ByteString -> Either String a) -> (a -> ByteString) -> a -> Property
prop_iso :: (Eq a, Show a) => (a -> b) -> (b -> a) -> a -> Property
prop_iso' :: (Eq a, Show a, Eq b) => (a -> b) -> (b -> a) -> a -> b -> Property
```

## REST API Testing

**Pattern from `test/unit/Chainweb/Test/RestAPI.hs`:**
```haskell
tests :: TestClientEnv -> TestTree
tests rdb = testGroup "REST API tests"
    [ testGroup "Http" (tests_ rdb False)
    , testGroup "Https" (tests_ rdb True)
    , withResource (newTestClientEnv rdb)
        $ \env -> testGroup "client session tests"
            [ testCase "headerClient" $ headerClientTest rdb
            ]
    ]
```

**Test server creation:**
```haskell
withTestAppServer :: Port -> Chainweb -> (TestClientEnv -> IO ()) -> IO ()
withChainwebTestServer :: (ClientEnv -> IO ()) -> IO ()
clientEnvWithChainwebTestServer :: (TestClientEnv -> IO ()) -> IO ()
```

## Test Coverage

**Requirements:** Not enforced (no minimum coverage target specified)

**View Coverage:**
```bash
cabal test --enable-coverage
# Coverage report in dist-newstyle/build/*/ghc-*/chainweb-*/hpc/
```

## Test Type Breakdown

**Unit Tests:**
- Scope: Single functions or small modules
- Approach: Property-based (QuickCheck) + unit cases (HUnit)
- Location: `test/unit/Chainweb/Test/`
- Examples: Encode/decode roundtrips, header validation

**Integration Tests:**
- Scope: Cross-module interactions, database operations
- Approach: Setup fixtures, perform operations, verify results
- Location: `test/unit/` with database resources
- Examples: BlockHeaderDb operations, CutDB synchronization

**E2E Tests:**
- Framework: Multi-node testing via `withNodes`
- Scope: Full node operation, block production, consensus
- Not separated into dedicated test suite; integrated into main suite

## Running Tests in Development

**Test in watch mode:**
Not supported by Tasty/Cabal directly. Use:
```bash
ghcid --command "cabal repl test:chainweb-tests"
```

**Run specific test:**
```bash
cabal test chainweb-tests -- --match "BlockHeader"
```

**Run with verbose output:**
```bash
cabal test chainweb-tests -- -v
```

**Debug single property:**
```haskell
-- In GHCI:
import Chainweb.Test.Roundtrips
quickCheck prop_encodeDecode
```

## Common Test Patterns

**Pattern: Setup -> Execute -> Verify**
```haskell
testCase "description" $ do
    -- Setup
    db <- testRocksDb
    let header = someBlockHeader

    -- Execute
    result <- insertBlockHeader db header

    -- Verify
    result @?= Success
```

**Pattern: Property with constraint**
```haskell
prop_something :: (Show a, Eq a) => a -> Property
prop_something x
    | isValid x = actualResult === expectedResult
    | otherwise = property True
```

**Pattern: Generate complex test data**
```haskell
forAll arbitraryComplexThing $ \thing ->
    let result = processedThing thing
    in result `satisfies` someConstraint
```

---

*Testing analysis: 2026-02-10*

{-# language LambdaCase #-}
{-# language NumericUnderscores #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language QuasiQuotes #-}
{-# language ViewPatterns #-}

-- | Stoa chain version definition. Uses devnet genesis payloads temporarily
-- (Phase 3 generates real ones).

module Chainweb.Version.Stoa(stoa, pattern Stoa) where

import qualified Data.Set as Set

import Chainweb.BlockCreationTime
import Chainweb.ChainId
import Chainweb.Difficulty
import Chainweb.Graph
import Chainweb.Time
import Chainweb.Utils
import Chainweb.Utils.Rule
import Chainweb.Version

import Pact.Types.Verifier

-- Temporary genesis payloads from devnet, until Phase 3 generates
-- Stoa-specific ones.
import qualified Chainweb.BlockHeader.Genesis.Development0Payload as DN0
import qualified Chainweb.BlockHeader.Genesis.Development1to19Payload as DNN

pattern Stoa :: ChainwebVersion
pattern Stoa <- ((== stoa) -> True) where
    Stoa = stoa

stoa :: ChainwebVersion
stoa = ChainwebVersion
    { _versionCode = ChainwebVersionCode 0x0000_000A
    , _versionName = ChainwebVersionName "stoa"
    , _versionForks = tabulateHashMap $ \case
        _ -> AllChains ForkAtGenesis
    , _versionUpgrades = AllChains mempty
    , _versionGraphs = Bottom (minBound, petersenChainGraph)
    , _versionBlockDelay = BlockDelay 30_000_000
    , _versionWindow = WindowWidth 120
    , _versionHeaderBaseSizeBytes = 318 - 110
    , _versionBootstraps = []
    , _versionGenesis = VersionGenesis
        { _genesisBlockTarget = AllChains $ HashTarget (maxBound `div` 100_000)
        , _genesisTime = AllChains $ BlockCreationTime [timeMicrosQQ| 2026-03-01T00:00:00.000000 |]
        , _genesisBlockPayload = onChains $ concat
            [ [(unsafeChainId 0, DN0.payloadBlock)]
            , [(unsafeChainId i, DNN.payloadBlock) | i <- [1..9]]
            ]
        }

    -- Hard cap at 500k gas per block (GAS-01)
    , _versionMaxBlockGasLimit = Bottom (minBound, Just 500_000)
    , _versionMinimumBlockHeaderHistory = Bottom (minBound, Nothing)
    , _versionCheats = VersionCheats
        { _disablePow = True
        , _fakeFirstEpochStart = True
        , _disablePact = False
        }
    , _versionDefaults = VersionDefaults
        { _disablePeerValidation = True
        , _disableMempoolSync = False
        }
    , _versionVerifierPluginNames = AllChains $ Bottom
        (minBound, Set.fromList $ map VerifierName ["hyperlane_v3_message", "allow", "signed_list"])
    , _versionQuirks = noQuirks
    , _versionForkNumber = 0
    }

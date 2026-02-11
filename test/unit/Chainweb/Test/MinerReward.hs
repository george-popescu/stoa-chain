{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Chainweb.Test.MinerReward
-- Copyright: Copyright © 2024 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
module Chainweb.Test.MinerReward
( tests
) where

import Chainweb.BlockHeight
import Chainweb.MinerReward
import Chainweb.Test.Orphans.Internal ()
import Chainweb.Version.Stoa

import Data.Decimal
import Data.Map.Strict qualified as M
import Data.Word

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

instance Arbitrary Stu where
    arbitrary = Stu <$> arbitrary

instance Arbitrary Kda where
    arbitrary = fmap Kda $ Decimal <$> choose (0,12) <*> arbitrary

newtype PositiveKda = PositiveKda { _positive :: Kda }
    deriving (Show, Eq, Ord)

instance Arbitrary PositiveKda where
    arbitrary = fmap (PositiveKda . Kda) $ Decimal
        <$> choose (0,12)
        <*> (getNonNegative <$> arbitrary)

tests :: TestTree
tests = testGroup "MinerReward"
    [ testProperty "kdaToStuToKda" prop_kdaToStuToKda
    , testProperty "stuToKdaToStu" prop_stuToKdaToStu
    , testCase "finalReward" test_finalMinerReward
    , testCase "minerRewardsMax" test_minerRewardsMax
    , testCase "minerRewardsFitWord64" test_minerRewardsFitWord64
    , testCase "expectedMinerRewardsHash" test_expectedMinerRewardsHash
    , testCase "expectedRawMinerRewardsHash" test_expectedRawMinerRewardsHash
    , testCase "stoaBlockRewardDividesByTen" test_stoaBlockRewardDividesByTen
    ]

-- --------------------------------------------------------------------------
-- Properties and Assertions

-- | The last non-zero entry in the STOA miner rewards CSV.
-- The terminal 0 entry is at height 127278720.
maxRewardHeight :: BlockHeight
maxRewardHeight = 127278720

prop_kdaToStuToKda :: PositiveKda -> Property
prop_kdaToStuToKda (PositiveKda kda) = stuToKda (kdaToStu kda) === kda

prop_stuToKdaToStu :: Stu -> Property
prop_stuToKdaToStu stu = kdaToStu (stuToKda stu) === stu

-- | Verify that rewards are 0 at and beyond the terminal block height.
--
test_finalMinerReward :: Assertion
test_finalMinerReward = do
    mapM_ rewardIsZero $ take 100 [maxRewardHeight..]
    mapM_ rewardIsZero $ take 10 [maxRewardHeight, (maxRewardHeight + 1000)..]
  where
    rewardIsZero h = assertEqual
        "The final miner reward is 0"
        (Kda 0)
        (minerRewardKda (blockMinerReward Stoa h))

-- | STOA maximum reward is ~4.605 (total across all chains).
-- Verify the maximum is bounded.
test_minerRewardsMax :: Assertion
test_minerRewardsMax = assertBool
    "maximum miner reward is smaller than 1e12 * 5"
    (_stu (maximum minerRewards) < 1e12 * 5)

test_minerRewardsFitWord64 :: Assertion
test_minerRewardsFitWord64 = assertBool
    "maximum miner reward fits into Word64"
    (_stu (maximum minerRewards) <= fromIntegral (maxBound @Word64))

test_expectedMinerRewardsHash :: Assertion
test_expectedMinerRewardsHash = assertEqual
    "expected miner rewards hash"
    expectedMinerRewardsHash
    (minerRewardsHash minerRewards)

test_expectedRawMinerRewardsHash :: Assertion
test_expectedRawMinerRewardsHash = assertEqual
    "expected raw miner rewards hash"
    expectedRawMinerRewardsHash
    (rawMinerRewardsHash rawMinerRewards)

-- | Verify that blockMinerReward divides the CSV value by 10 (Petersen graph
-- order) for the Stoa version. The first CSV entry has total reward
-- ~4.604266176 STOA across all chains. Per chain: ~0.460426617 STOA.
--
test_stoaBlockRewardDividesByTen :: Assertion
test_stoaBlockRewardDividesByTen = do
    -- Get the per-chain reward at height 0
    let perChain = blockMinerReward Stoa 0
    -- Get the total (CSV) reward at height 0
    case M.lookupGE (BlockHeight 0) minerRewards of
        Nothing -> assertFailure "minerRewards table is empty"
        Just (_, totalStu) -> do
            -- Per chain should be total / 10
            let expected = divideStu totalStu 10
            assertEqual
                "blockMinerReward Stoa 0 == CSV total / 10 (Petersen graph order)"
                (MinerReward expected)
                perChain

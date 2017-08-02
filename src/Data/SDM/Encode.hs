-- | Provides encoding algorithms for n-dimensional 
-- scalar data.
--
module Data.SDM.Encode
  ( FeatureSet
  , rLin
  , rLog
  , e1i
  , eNi
  , fuse
  ) where

import Data.List

--import Data.SpaceFillingCurve.Hilbert.Integer
import System.Random

-- | Represents a set of features.
type FeatureSet = (Int, [Int])

-- Pre-Processing
-- ****************************************************
-- | Linearly resolve a list of values to a list of
-- bucket indices based on a resolution r.
rLin :: (RealFrac r) => r -> [r] -> [Int]
rLin r xs = map (\x -> round $ x / r) xs

-- | Log n resolve a list of values to a list of
-- bucket indices.
rLog :: (Floating a, RealFrac a) => a -> [a] -> [Int]
rLog n xs = map (round . logBase n) xs

-- Encoding
-- ****************************************************
-- | Random chunking.
rblk :: Int -> Int -> [Int]
rblk n s =
  let g = mkStdGen s
      r = randomRs (1, n) g
  in take 1024 r

-- | Bucket generation algorithm. Simple mapping of
-- integers onto a deterministic uniformly distributed
-- integer k in {1 <= k <= n}.
rand :: Int -> Int -> Int
rand n x =
  let blk = floor $ (fromIntegral x) / 1024
      r = mod x 1024
  in (rblk n blk) !! r

-- | Encode an integer to a feature set.
e1i :: Int -> Int -> FeatureSet
e1i n x =
  let w = round $ (fromIntegral n) * 0.1
  in (n, nub $ map (rand n) [x .. x + w])

-- | Encode n integers to a feature set. The provided
-- data is first mapped onto a single dimension through a 
-- hilbert transform, then mapped onto feature space.
eNi :: Int -> [Int] -> FeatureSet
eNi n xs =
  let w = round $ (fromIntegral n) * 0.1
      h = sum -- hilbert 64
  in (n, nub $ map (rand n) [h xs .. h xs + w])

-- Post-Processing
-- ****************************************************
-- | Fuse 2 feature sets.
fuseT :: FeatureSet -> FeatureSet -> FeatureSet
fuseT a b =
  let aa = map (+ fst b) $ snd a
  in (fst a + fst b, snd b ++ aa)

-- | Fuse multiple feature sets.
fuse :: [FeatureSet] -> FeatureSet
fuse xs =
  let (n, xn) = foldr fuseT (0, []) xs
  in (n, sort xn)

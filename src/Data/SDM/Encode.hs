-- | Provides encoding algorithms for n-dimensional 
-- scalar data.
--
module Data.SDM.Encode
  ( FSet
  , rLin
  , rLog
  , e1i
  , eNi
  , fuseT
  , fuse
  ) where

--import Data.SpaceFillingCurve.Hilbert.Integer
import qualified Data.Set as S
import Data.Set (Set)
import System.Random

-- | Represents a set of features with an upper bound.
data FSet =
  FSet Int -- upper bound
       (Set Int) -- active features
  deriving (Show, Eq)

-- | Linearly resolve a list of values to a list of
-- bucket indices based on a resolution r.
rLin :: (RealFrac r) => r -> [r] -> [Int]
rLin r xs = map (\x -> round $ x / r) xs

-- | Log n resolve a list of values to a list of
-- bucket indices.
rLog :: (Floating a, RealFrac a) => a -> [a] -> [Int]
rLog n xs = map (round . logBase n) xs

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
e1i :: Int -> Int -> FSet
e1i n x =
  let w = round $ (fromIntegral n) * 0.1
  in FSet n (S.fromList $ map (rand n) [x .. x + w])

-- | Encode n integers to a feature set. The provided
-- data is first mapped onto a single dimension through a 
-- hilbert transform, then mapped onto feature space.
eNi :: Int -> [Int] -> FSet
eNi n xs =
  let w = round $ (fromIntegral n) * 0.1
      h = sum -- hilbert 64
  in FSet n (S.fromList $ map (rand n) [h xs .. h xs + w])

-- | Fuse an FSet with another FSet.
fuseT :: FSet -> FSet -> FSet
fuseT (FSet ab axs) (FSet bb bxs) =
  let aa = S.map (+ bb) axs
  in FSet (ab + bb) (S.union aa bxs)

-- | Fuse a list of FSets.
fuse :: [FSet] -> FSet
fuse xs = foldr fuseT (FSet 0 S.empty) xs

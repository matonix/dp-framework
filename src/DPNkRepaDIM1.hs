{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}

module DPNkRepaDIM1 where
import Debug.Trace
import qualified Data.Vector                   as V
import qualified Data.Vector.Mutable           as VM
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as VUM
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as VGM
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.ByteString.Builder       as BSB
import           Data.Char                      ( isSpace )
import           Data.List                      ( unfoldr )
import Data.Ix
import Prelude as P
import Data.Array.Repa as A
import Data.Array.Repa.Repr.Vector (V)

{-# ANN module "HLint: ignore Eta reduce" #-}

main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  print $ dp xs (0, 100000) (n-1) -- 0-indexed

-- >>> dp (VU.fromList [10, 30, 40, 20]) (0, 100000) 3
-- 30
dp :: VU.Vector Int -> (Int, Int) -> Int -> Int
dp h (lb, ub) n = -- traceShow (map dp $ range lb ub) $
  memo A.! ix1 n
  where
    cost i j = abs $ (h VU.! i) - (h VU.! j)
    f 0 = 0
    f 1 = cost 0 1
    f (i+2) = min (f' (i+1) + cost (i+1) (i+2)) (f' i + cost i (i+2))
    f' i = memo A.! ix1 i
    memo :: Array V DIM1 Int
    memo = computeS $ A.fromFunction (ix1 ub) $ ix1Func f
    ix1Func :: (Int -> Int) -> DIM1 -> Int
    ix1Func g = \(Z:.i) -> g i

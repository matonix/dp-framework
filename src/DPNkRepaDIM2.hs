{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}

module DPNkRepaDIM2 where
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

-- https://atcoder.jp/contests/dp/tasks/dp_d
main :: IO ()
main = do
  [n, maxW] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  mat <- VU.replicateM n $ (\v -> (v VU.! 0, v VU.! 1)) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (ws, vs) = VU.unzip mat  
  print $ dp ws vs n maxW -- 0-indexed

-- >>> dp (VU.fromList [3,4,5]) (VU.fromList [30,50,60]) 3 8
-- 90
dp :: VU.Vector Int -> VU.Vector Int -> Int -> Int -> Int
dp ws vs n maxW = -- traceShow memo $
  memo A.! ix2 n maxW
  where
    f 0 _ = 0
    f (i+1) w 
      | w >= ws VU.! i = (f' i (w - ws VU.! i) + vs VU.! i) `max` f' i w
      | otherwise = f' i w
    f' i w = memo A.! ix2 i w
    memo :: Array V DIM2 Int
    memo = computeS $ A.fromFunction (ix2 (n+1) (maxW+1)) $ ix2Func f
    ix2Func g = \(Z:.i:.j) -> g i j

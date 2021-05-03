{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}

module DPNkMAExamples where

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
import Control.Applicative (liftA2)
import Control.Monad.ST (ST)
import Data.Ix
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Debug.Trace

-- https://atcoder.jp/contests/dp/tasks/dp_d
main :: IO ()
main = do
  [n, maxW] <- unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  mat <- VU.replicateM n $ (\v -> (v VU.! 0, v VU.! 1)) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (ws, vs) = VU.unzip mat  
  print $ knapsack ws vs ((0, 0), (n, maxW)) 0 (n, maxW)

-- >>> knapsack (VU.fromList [3,4,5]) (VU.fromList [30,50,60]) ((0, 0), (3, 8)) 0 (3, 8)
-- 90
knapsack :: VU.Vector Int -> VU.Vector Int -> ((Int, Int), (Int, Int)) -> Int -> (Int, Int) -> Int
knapsack weight value lub e (n, w) = -- traceShow dp $ 
  dp ! (n, w)
  where
    f (0, w) = 0
    f (i + 1, w)
      | w >= weight VU.! i = liftR2 max (f' (i, w - (weight VU.! i)) + mkR (value VU.! i)) (f' (i, w))
      | otherwise = f' (i, w)

    dp = runSTUArray $ do
      arr <- newArray lub e
      let actions = [i @= f i | i <- range lub]
      foldl wplus wzero actions `run` arr
      return arr

newtype R a m i e = R {get :: a i e -> m e}

newtype W a m i e = W {run :: a i e -> m ()}

mkR :: Monad m => e -> R a m i e
mkR e = R $ \arr -> do return e

toR :: Monad m => (d -> e) -> d -> R a m i e
toR f e = R $ \arr -> do
  return $ f e

liftR :: Monad m => (e -> e) -> R a m i e -> R a m i e
liftR f a = R $ \arr -> do
  v <- a `get` arr
  return $ f v

liftR2 :: Monad m => (e -> e -> e) -> R a m i e -> R a m i e -> R a m i e
liftR2 f a b = R $ \arr -> do
  va <- a `get` arr
  vb <- b `get` arr
  return $ f va vb

(@=) :: (MArray a e m, Ix i) => i -> R a m i e -> W a m i e
i @= a = W $ \arr -> do
  e <- a `get` arr
  writeArray arr i e
infixr 0 @=

wzero :: Monad m => W a m i e
wzero = W $ \arr -> do return ()

wplus :: Monad m => W a m i e -> W a m i e -> W a m i e
wplus f g = W $ \arr -> do
  f `run` arr
  g `run` arr

f' :: (MArray a e m, Ix i) => i -> R a m i e
f' i = R $ \arr -> do readArray arr i

instance (Num e, Monad m) => Num (R a m i e) where
  (+) = liftR2 (+)
  (*) = liftR2 (*)
  abs = liftR abs
  signum = liftR signum
  fromInteger = toR fromInteger
  negate = liftR negate

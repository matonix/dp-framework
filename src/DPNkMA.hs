{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TypeApplications #-}

{-# LANGUAGE FlexibleContexts #-}
module DPNkMA where

import Control.Applicative (liftA2)
import Control.Monad.ST (ST)
import Data.Ix
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import Debug.Trace

-- 方針
-- memo 配列からの参照、ではなく、mutable arrayへの書き込み操作を生成する
-- f の再帰呼び出しを配列からの読み込みとしてエンコード (R)
-- f の実行結果を配列への書き込みへエンコード (W)
-- 答えの呼び出し前に、ある順番で配列を舐める

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

-- >>> fib (0, 1000) 0 50
-- 20365011074
fib :: (Int, Int) -> Int -> Int -> Int
fib lub e n = dp ! n
  where
    f 0 = 1
    f 1 = 1
    f (n + 2) = f' n + f' (n + 1)

    dp = runSTUArray $ do
      arr <- newArray lub e
      let actions = [i @= f i | i <- range lub]
      foldl wplus wzero actions `run` arr
      return arr

-- >>> knapsack ((0, 0), (6, maxW)) 0 (6, maxW)
-- 94
knapsack :: ((Int, Int), (Int, Int)) -> Int -> (Int, Int) -> Int
knapsack lub e (n, w) = -- traceShow dp $ 
  dp ! (n, w)
  where
    f (0, w) = 0
    f (i + 1, w)
      | w >= weight ! i = liftR2 max (f' (i, w - (weight ! i)) + mkR (value ! i)) (f' (i, w))
      | otherwise = f' (i, w)

    dp = runSTUArray $ do
      arr <- newArray lub e
      let actions = [i @= f i | i <- range lub]
      foldl wplus wzero actions `run` arr
      return arr

value :: UArray Int Int
value = listArray (0, 5) [3, 2, 6, 1, 3, 85]

weight :: UArray Int Int
weight = listArray (0, 5) [2, 1, 3, 2, 1, 5]

maxW :: Int
maxW = 9

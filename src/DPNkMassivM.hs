{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TypeApplications #-}

module DPNkMassivM where

import Control.Applicative (liftA2)
import Control.Monad.ST (ST)
import Data.Massiv.Array as A
import qualified Data.Vector.Unboxed as VU
import Debug.Trace
import Prelude as P

-- 方針
-- memo 配列からの参照、ではなく、mutable arrayへの書き込み操作を生成する
-- f の再帰呼び出しを配列からの読み込みとしてエンコード (R)
-- f の実行結果を配列への書き込みへエンコード (W)
-- 答えの呼び出し前に、ある順番で配列を舐める

newtype DPRead a m i e = R {get :: MArray (PrimState m) a i e -> m e}

newtype DPWrite a m i e = W {run :: MArray (PrimState m) a i e -> m ()}

mkR :: Monad m => e -> DPRead a m i e
mkR e = R $ \arr -> do return e

toR :: Monad m => (d -> e) -> d -> DPRead a m i e
toR f e = R $ \arr -> do
  return $ f e

liftR :: Monad m => (e -> e) -> DPRead a m i e -> DPRead a m i e
liftR f a = R $ \arr -> do
  v <- a `get` arr
  return $ f v

liftR2 :: Monad m => (e -> e -> e) -> DPRead a m i e -> DPRead a m i e -> DPRead a m i e
liftR2 f a b = R $ \arr -> do
  va <- a `get` arr
  vb <- b `get` arr
  return $ f va vb

(@=) :: (Mutable a i e, PrimMonad m) => i -> DPRead a m i e -> DPWrite a m i e
i @= a = W $ \arr -> do
  e <- a `get` arr
  write_ arr i e

infixr 0 @=

mkW :: (Mutable a i e, PrimMonad m) => (i -> DPRead a m i e) -> MArray (PrimState m) a i e -> i -> m ()
mkW f arr i = do
  e <- f i `get` arr
  write_ arr i e

f' :: (Mutable a i e, PrimMonad m, MonadThrow m) => i -> DPRead a m i e
f' i = R $ \arr -> do readM arr i

instance (Num e, Monad m) => Num (DPRead a m i e) where
  (+) = liftR2 (+)
  (*) = liftR2 (*)
  abs = liftR abs
  signum = liftR signum
  fromInteger = toR fromInteger
  negate = liftR negate

-- >>> fib (Ix1 0) (Sz1 1000) 0 30
-- 1346269
fib :: Ix1 -> Sz1 -> Int -> Ix1 -> Int
fib ix sz e n = dp ! n
  where
    f 0 = 1
    f 1 = 1
    f (n + 2) = f' n + f' (n + 1)

    dp = createArrayST_ @U sz $ \arr -> A.mapM_ (mkW f arr) $ rangeSize Seq ix sz

-- 注意: (Sz2 6 maxW + 1) = Sz (7 :. 10)
-- >>> knapsack (Ix2 0 0) (Sz2 6 maxW + 1) 0 (Ix2 6 maxW)
-- 94
knapsack :: Ix2 -> Sz2 -> Int -> Ix2 -> Int
knapsack ix sz e ans =
  -- traceShow dp $
  dp ! ans
  where
    f (0 :. w) = 0
    f (i + 1 :. w)
      | w >= weight VU.! i = liftR2 max (f' (i :. w - (weight VU.! i)) + mkR (value VU.! i)) (f' (i :. w))
      | otherwise = f' (i :. w)

    dp = createArrayST_ @U sz $ \arr -> A.mapM_ (mkW f arr) $ rangeSize Seq ix sz

value :: VU.Vector Int
value = VU.fromList [3, 2, 6, 1, 3, 85]

weight :: VU.Vector Int
weight = VU.fromList [2, 1, 3, 2, 1, 5]

maxW :: Int
maxW = 9

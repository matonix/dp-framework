{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

module DPNkMassivMExamples where

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
import Data.Massiv.Array as A
import Debug.Trace
import Prelude as P

-- https://atcoder.jp/contests/dp/tasks/dp_d
-- 187 ms, 82984 KB, AC
main :: IO ()
main = do
  [n, maxW] <- Data.List.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  mat <- VU.replicateM n $ (\v -> (v VU.! 0, v VU.! 1)) . VU.unfoldr (BS.readInt . BS.dropWhile isSpace) <$> BS.getLine
  let (ws, vs) = VU.unzip mat  
  print $ knapsack ws vs (Ix2 0 0) (Sz2 n maxW + 1) 0 (Ix2 n maxW)

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

wzero :: Monad m => DPWrite a m i e
wzero = W $ \arr -> do return ()

wplus :: Monad m => DPWrite a m i e -> DPWrite a m i e -> DPWrite a m i e
wplus f g = W $ \arr -> do
  f `run` arr
  g `run` arr

f' :: (Mutable a i e, PrimMonad m, MonadThrow m) => i -> DPRead a m i e
f' i = R $ \arr -> do readM arr i

instance (Num e, Monad m) => Num (DPRead a m i e) where
  (+) = liftR2 (+)
  (*) = liftR2 (*)
  abs = liftR abs
  signum = liftR signum
  fromInteger = toR fromInteger
  negate = liftR negate

-- 注意: (Sz2 6 9 + 1) = Sz (7 :. 10)
-- >>> knapsack (VU.fromList [3,4,5]) (VU.fromList [30,50,60]) (Ix2 0 0) (Sz2 3 8 + 1) 0 (Ix2 3 8)
-- 90
knapsack :: VU.Vector Int -> VU.Vector Int -> Ix2 -> Sz2 -> Int -> Ix2 -> Int
knapsack weight value ix sz e ans =
  -- traceShow dp $
  dp ! ans
  where
    f (0 :. w) = 0
    f (i + 1 :. w)
      | w >= weight VU.! i = liftR2 max (f' (i :. w - (weight VU.! i)) + mkR (value VU.! i)) (f' (i :. w))
      | otherwise = f' (i :. w)

    dp = createArrayST_ @U sz $ \arr -> A.mapM_ (\i -> (i @= f i) `run` arr) $ rangeSize Seq ix sz
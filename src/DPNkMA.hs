{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}
{-# LANGUAGE TypeApplications #-}

module DPNkMA where

import Control.Applicative (liftA2)
import Data.Array
import Data.Array.MArray
import Debug.Trace
import Control.Monad.ST (ST)
import Data.Array.ST
import qualified Data.Array.Unboxed as U

-- 方針
-- memo 配列からの参照、ではなく、mutable arrayへの書き込み操作を生成する
-- f の再帰呼び出しを配列からの読み込みとしてエンコード
-- f の実行結果を配列への書き込みへエンコード
-- 答えの呼び出し前に、ある順番で配列を舐める

{-# ANN module "HLint: ignore Eta reduce" #-}

newtype A a m i e = A { get :: a i e -> m e }
newtype W a m i e = W { run :: a i e -> m () }

instance (Num e, Monad m) => Num (A a m i e) where
  a + b = A $ \arr -> do
    va <- a `get` arr
    vb <- b `get` arr
    return $ va + vb
  a * b = A $ \arr -> do
    va <- a `get` arr
    vb <- b `get` arr
    return $ va * vb
  abs a = A $ \arr -> do
    v <- a `get` arr
    return $ abs v
  signum a = A $ \arr -> do
    v <- a `get` arr
    return $ signum v
  fromInteger a = A $ \arr -> do
    return $ fromInteger a
  negate a = A $ \arr -> do
    v <- a `get` arr
    return $ negate v

-- >>> fib 50
-- 20365011074
fib :: Int -> Int
fib n = dp U.! n
  where
    f :: Int -> W (STUArray s) (ST s) Int Int
    f 0 = 0 @= 1
    f 1 = 1 @= 1
    f (n + 2) = n + 2 @= f' n + f' (n + 1)

    f' :: (MArray a e m, Ix i) => i -> A a m i e
    f' i = A $ \arr -> do readArray arr i

    (@=) :: Int -> A (STUArray s) (ST s) Int Int -> W (STUArray s) (ST s) Int Int
    i @= a = W $ \arr -> do 
      e <- a `get` arr
      writeArray arr i e 
    infixr 1 @=

    (>>>>) :: Monad m => W a m i e -> W a m i e -> W a m i e
    f >>>> g = W $ \arr -> do
      f `run` arr
      g `run` arr

    zero :: Monad m => W a m i e
    zero = W $ \arr -> do return ()

    dp :: U.UArray Int Int
    dp = runSTUArray $ do
      arr <- newArray (lb, ub) 0
      let actions = [f i | i <- [lb .. ub]]
      let action = foldl (>>>>) zero actions
      action `run` arr
      return arr

    lb :: Int
    lb = 0

    ub :: Int
    ub = 1000

knapsackAns :: Int
knapsackAns = knapsack 6 maxW
  where
    knapsack :: Int -> Int -> Int
    knapsack 0 w = 0
    knapsack (i + 1) w
      | w >= weight ! i = knapsack' i (w - (weight ! i)) + value ! i `max` knapsack' i w
      | otherwise = knapsack' i w

    knapsack' = curry (memo !)
    memo = listArray (lb, ub) [knapsack i w | (i, w) <- range (lb, ub)]

    lb :: (Int, Int)
    lb = (0, 0)

    ub :: (Int, Int)
    ub = (1000, 1000)

    value :: Array Int Int
    value = listArray (0, 5) [3, 2, 6, 1, 3, 85]

    weight :: Array Int Int
    weight = listArray (0, 5) [2, 1, 3, 2, 1, 5]

    maxW :: Int
    maxW = 9

main :: IO ()
main = do
  n <- readLn :: IO Int
  h <- listArray (1, n) . map (read @Int) . words <$> getLine
  print $ frog1Ans' h n

frog1Ans :: Array Int Int -> Int -> Int
frog1Ans h n =
  -- traceShow (map dp [1..n]) $
  dp n
  where
    cost i j = abs $ (h ! i) - (h ! j)

    dp 1 = 0
    dp 2 = cost 1 2
    dp (i + 2) = min (dp' (i + 1) + cost (i + 1) (i + 2)) (dp' i + cost i (i + 2))

    dp' = (memo !)
    memo = listArray (lb, ub) [dp i | i <- range (lb, ub)]

    lb = 1
    ub = 100000

frog1Ans' :: Array Int Int -> Int -> Int
frog1Ans' h n =
  -- traceShow (map dp [1..n]) $
  dp n
  where
    cost i j = abs $ (h ! i) - (h ! j)

    dp 1 = 0
    dp 2 = cost 1 2
    dp i = min (dp' (i -1) + cost (i -1) i) (dp' (i -2) + cost (i -2) i)

    dp' = (memo !)
    memo = listArray (lb, ub) [dp i | i <- range (lb, ub)]

    lb = 1
    ub = 100000

-- May need: NPlusKPatterns, curry (2dim), additional data in arguments
dp :: Ix i => (i, i) -> i -> e
dp (lb, ub) n =
  -- traceShow (map dp $ range lb ub) $
  f n
  where
    f = undefined
    f' = (memo !)
    memo = listArray (lb, ub) $ map f $ range (lb, ub)

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NPlusKPatterns #-}

module DPNk where
import Data.Array
import Debug.Trace

{-# ANN module "HLint: ignore Eta reduce" #-}

fibAns :: Int
fibAns = fib 30
  where
    fib :: Int -> Int
    fib 0 = 1
    fib 1 = 1
    fib (n+2) = fib' n + fib' (n+1)

    fib' :: Int -> Int
    fib' = (memo !)

    memo :: Array Int Int
    memo = listArray (lb, ub) [fib i | i <- [lb .. ub]]

    lb :: Int
    lb = 0

    ub :: Int
    ub = 1000

knapsackAns :: Int
knapsackAns = knapsack 6 maxW
  where
    knapsack :: Int -> Int -> Int
    knapsack 0 w = 0
    knapsack (i+1) w
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
frog1Ans h n = -- traceShow (map dp [1..n]) $ 
  dp n
  where
    cost i j = abs $ (h ! i) - (h ! j)

    dp 1 = 0
    dp 2 = cost 1 2
    dp (i+2) = min (dp' (i+1) + cost (i+1) (i+2)) (dp' i + cost i (i+2))
    
    dp' = (memo !)
    memo = listArray (lb, ub) [dp i | i <- range (lb, ub)]

    lb = 1
    ub = 100000

frog1Ans' :: Array Int Int -> Int -> Int
frog1Ans' h n = -- traceShow (map dp [1..n]) $ 
  dp n
  where
    cost i j = abs $ (h ! i) - (h ! j)

    dp 1 = 0
    dp 2 = cost 1 2
    dp i = min (dp' (i-1) + cost (i-1) i) (dp' (i-2) + cost (i-2) i)
    
    dp' = (memo !)
    memo = listArray (lb, ub) [dp i | i <- range (lb, ub)]

    lb = 1
    ub = 100000

-- May need: NPlusKPatterns, curry (2dim), additional data in arguments
dp :: Ix i => (i, i) -> i -> e
dp (lb, ub) n = -- traceShow (map dp $ range lb ub) $
  f n
  where
    f = undefined 
    f' = (memo !)
    memo = listArray (lb, ub) $ map f $ range (lb, ub)
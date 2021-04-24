{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module DPGenExamples where

import Data.Array
import Data.Array.ST.Safe
import Data.Function
import Data.List
import Debug.Trace
import Data.Maybe

type Dfn i e = (Ini i e, Eqn i e)

newtype Rec i = Rec (i -> i)

data Eqn i e = Rec i := [(Expr i e, Cond i e)]

(##) :: Expr i e -> Cond i e -> (Expr i e, Cond i e)
a ## b = (a, b)

data Cond i e
  = Otherwise
  | Match i
  | If (i -> Bool)

data Expr i e
  = Val e
  | DP (i -> i)
  | ItoE (i -> e)
  | UnOp (e -> e) (Expr i e)
  | BinOp (e -> e -> e) (Expr i e) (Expr i e)

newtype Ini i e = Ini (i -> Maybe e)

-- |
-- interpret は可変配列を生成して eval を呼び出し、最終的にできた配列から解を得る
interpret :: (Ix i, Show i) => Dfn i e -> (i, i) -> i -> e
interpret dfn@(Ini icond, Rec idx := rhss) lu i = mkArr dfn lu ! i

mkArr :: (Ix i, Show i) => Dfn i e -> (i, i) -> Array i e
mkArr (Ini icond, eqn) lu = runSTArray $ do
  let luEx = traceShowId $ calcExtraOffset eqn lu
  let (is, rs) = traceShowId $ partition (isJust . icond) $ range lu
  marr <- newArray_ luEx 
  sequence_ [traceShow ("init " ++ show i) $ writeArray marr i e | i <- is, let Just e = icond i]
  sequence_ [traceShow ("eval " ++ show i) $ eval eqn marr i | i <- rs]
  return marr

-- |
-- Index out of range しないために必要な配列のインデックスの範囲を計算
calcExtraOffset :: Ix i => Eqn i e -> (i, i) -> (i, i)
calcExtraOffset (Rec idx := rhss) (lb, ub) =
  (minimum (lb : map (lb &) offsets), maximum (ub : map (ub &) offsets))
  where
    offsets = idx : [r | DP r <- concatMap (flatten . fst) rhss]

flatten :: Expr i e -> [Expr i e]
flatten expr = case expr of
  Val v -> [Val v]
  DP f -> [DP f]
  ItoE f -> [ItoE f]
  UnOp u e1 -> UnOp u e1 : flatten e1
  BinOp b e1 e2 -> BinOp b e1 e2 : flatten e1 ++ flatten e2

-- |
-- eval は引数のインデックス i を match で場合分けし、分岐先のケースを calc で計算する（先ほどの interpret）
eval :: (MArray a e m, Ix i) => Eqn i e -> a i e -> i -> m ()
eval (Rec idx := rhss) arr i =
  case find (match i . snd) rhss of
    Just aCase -> do
      e <- calc arr i (fst aCase)
      writeArray arr (idx i) e
    Nothing -> error "Failed to match in RHS."

match :: Eq i => i -> Cond i a -> Bool
match i cond = case cond of
  Otherwise -> True
  Match j -> i == j
  If p -> p i

calc :: (MArray a e m, Ix i) => a i e -> i -> Expr i e -> m e
calc arr i expr = case expr of
  Val v -> return v
  DP f -> readArray arr (f i)
  ItoE f -> return $ f i
  UnOp u e1 -> u <$> calc arr i e1
  BinOp b e1 e2 -> b <$> calc arr i e1 <*> calc arr i e2

-- * utils

(+#) :: Num e => Expr i e -> Expr i e -> Expr i e
(+#) = BinOp (+)

(-#) :: Num e => Expr i e -> Expr i e -> Expr i e
(-#) = BinOp (-)

maxE :: (Num e, Ord e) => Expr i e -> Expr i e -> Expr i e
maxE = BinOp max

minE :: (Num e, Ord e) => Expr i e -> Expr i e -> Expr i e
minE = BinOp min

-- * Examples

-- >>> interpret fib (0, 30) 30
-- 1346269
fib :: Dfn Int Int
fib =
  ( Ini $
      \case
        0 -> Just 1
        1 -> Just 1
        _ -> Nothing,
    Rec (+ 2)
      := [DP (+ 1) +# DP id ## Otherwise]
  )

-- >>> interpret fib (0, 30) 30
-- 1346269
fib2 :: Dfn Int Int
fib2 =
  ( Ini $
      \case
        0 -> Just 1
        1 -> Just 1
        _ -> Nothing,
    Rec id
      := [DP (\i -> i - 1) +# DP (\i -> i - 2) ## Otherwise]
  )

-- |
-- https://qiita.com/drken/items/a5e6fe22863b7992efdb#%E5%95%8F%E9%A1%8C-2%E3%83%8A%E3%83%83%E3%83%97%E3%82%B5%E3%83%83%E3%82%AF%E5%95%8F%E9%A1%8C
-- >>> interpret knapsack ((0, 0), (5, maxW)) (6, maxW)
-- 94
knapsack :: Dfn (Int, Int) Int
knapsack =
  ( Ini $ const $ Just 0,
    Rec (\(i, w) -> (i + 1, w))
      := [ DP (\(i, w) -> (i, w - weight ! i)) +# ItoE (\(i, w) -> value ! i) `maxE` DP id ## If (\(i, w) -> w >= weight ! i),
           DP id ## Otherwise
         ]
  )

value :: Array Int Int
value = listArray (0, 5) [3, 2, 6, 1, 3, 85]

weight :: Array Int Int
weight = listArray (0, 5) [2, 1, 3, 2, 1, 5]

maxW :: Int
maxW = 9

-- >>> interpret subsetsum ((0, 0), (n, sumA)) (n+1, sumA)
-- True
subsetsum :: Dfn (Int, Int) Bool
subsetsum =
  ( Ini $
      \(i, j) ->
        case (i, j) of
          (0, 0) -> Just True
          _ -> Nothing,
    Rec (\(i, j) -> (i + 1, j))
      := [ DP (\(i, j) -> (i, j - a ! i)) ||# DP id ## If (\(i, j) -> j >= a ! i),
           DP id ## Otherwise
         ]
  )
  where
    a ||# b = BinOp (||) a b

n :: Int
n = 2

a :: Array Int Int
a = listArray (0, n) [7, 5, 3]

sumA :: Int
sumA = 10

-- * 残課題

-- - パフォーマンスを計測する（大事）
-- - もう少し書きやすくする（特にラムダ抽象）
-- - 表現力を確認する（書けないDPはないか？）

-- |
-- 二通りで書いたときに、どちらの書き方でも動作するのが理想

main :: IO ()
main = do
  n <- readLn :: IO Int
  h <- listArray (1, n) . map (read @Int) . words <$> getLine
  print $ interpret (frog1 cost h) (3, n) n
  print $ interpret (frog1' cost h) (3, n) n

-- print $ mkArr (frog1' cost h) (3, n)

cost h i j = abs $ (h ! i) - (h ! j)

-- c_1 = 0
-- c_2 = cost 1 2
-- c_i+2 = c_i+1 + cost (i+1) (i+2) `min` c_i + cost i (i+2)
-- c_N = ?
frog1 cost h =
  ( Ini $ \case
      1 -> Just 0
      2 -> Just $ cost h 1 2
      _ -> Nothing,
    Rec (+ 2) := [minE (DP (+ 1) +# ItoE (\i -> cost h (i + 1) (i + 2))) (DP id +# ItoE (\i -> cost h i (i + 2))) ## Otherwise]
  )

-- c_1 = 0
-- c_2 = cost 1 2
-- c_i = c_i-1 + cost i (i-i) `min` c_i-2 + cost i (i-2)
-- c_N = ?
frog1' cost h =
  ( Ini $ \case
      1 -> Just 0
      2 -> Just $ cost h 1 2
      _ -> Nothing,
    Rec id := [DP (\i -> i - 1) +# ItoE (\i -> cost h i (i - 1)) `minE` DP (\i -> i - 2) +# ItoE (\i -> cost h i (i - 2)) ## Otherwise]
  )

-- 課題
-- ini と rec を統一的に扱う
-- 与えた探索範囲 range を ini で処理する部分と rec で処理する部分に二分して、 overlap を無くさないと、 body 部分の添字が想定外になったりする（上記だと cost の計算で配列外参照が起きる

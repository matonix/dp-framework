{-# LANGUAGE LambdaCase #-}

module DPGen where

import Data.Array
import Data.Array.ST.Safe
  ( MArray,
    newListArray,
    readArray,
    runSTArray,
    writeArray,
  )
import Data.Function
import Data.List

data Dfn i e = Dfn
  { ini :: Ini i e,
    rec :: Eqn i e
  }

newtype Idx i = Idx (i -> i)

data Eqn i e = Idx i := [(Expr i e, Cond i e)]

(|-|) :: Expr i e -> Cond i e -> (Expr i e, Cond i e)
a |-| b = (a, b)

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

newtype Ini i e = Ini (i -> e)

-- |
-- interpret は可変配列を生成して eval を呼び出し、最終的にできた配列から解を得る
interpret :: (Ix i) => Dfn i e -> (i, i) -> i -> e
interpret dfn@(Dfn (Ini icond) (Idx idx := rhss)) lu i =
  let arr = runSTArray $ do
        let luEx = calcExtraOffset dfn lu
        marr <- newListArray luEx $ map icond $range luEx
        sequence_ [eval dfn marr i | i <- range lu]
        return marr
   in arr ! i

-- |
-- Index out of range しないために必要な配列のインデックスの範囲を計算
calcExtraOffset :: Ix i => Dfn i e -> (i, i) -> (i, i)
calcExtraOffset (Dfn _ (Idx idx := rhss)) (lb, ub) =
  (minimum (lb : map (lb &) offsets), maximum (ub : map (ub &) offsets))
  where
    offsets = idx : [r | DP r <- map fst rhss]

-- |
-- eval は引数のインデックス i を match で場合分けし、分岐先のケースを calc で計算する（先ほどの interpret）
eval :: (MArray a e m, Ix i) => Dfn i e -> a i e -> i -> m ()
eval (Dfn _ (Idx idx := rhss)) arr i =
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

-- * Examples

-- >>> interpret fib (0, 30) 30
-- 1346269
fib :: Dfn Int Int
fib =
  Dfn
    { ini = Ini $ \case
        0 -> 1
        1 -> 1
        _ -> 0,
      rec =
        Idx (+ 2) := [DP (+ 1) +# DP id |-| Otherwise]
    }

-- |
-- https://qiita.com/drken/items/a5e6fe22863b7992efdb#%E5%95%8F%E9%A1%8C-2%E3%83%8A%E3%83%83%E3%83%97%E3%82%B5%E3%83%83%E3%82%AF%E5%95%8F%E9%A1%8C
-- >>> interpret knapsack ((0, 0), (5, maxW)) (6, maxW)
-- 94
knapsack :: Dfn (Int, Int) Int
knapsack =
  Dfn
    { ini = Ini $ const 0,
      rec =
        Idx (\(i, w) -> (i + 1, w))
          := [ DP (\(i, w) -> (i, w - weight ! i)) +# ItoE (\(i, w) -> value ! i) `maxE` DP id |-| If (\(i, w) -> w >= weight ! i),
               DP id |-| Otherwise
             ]
    }

value :: Array Int Int
value = listArray (0, 5) [3, 2, 6, 1, 3, 85]

weight :: Array Int Int
weight = listArray (0, 5) [2, 1, 3, 2, 1, 5]

maxW :: Int
maxW = 9

-- * 残課題

-- - パフォーマンスを計測する（大事）
-- - もう少し書きやすくする（特にラムダ抽象）
-- - 表現力を確認する（書けないDPはないか？）
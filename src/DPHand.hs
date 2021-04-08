{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DPHand where

import Data.Array
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"

lb :: Int
lb = 0

ub :: Int
ub = 1000

f :: Int -> Int
f 0 = 1
f 1 = 1
f n = f (n - 1) + f (n - 2)

f' :: Int -> Int
f' 0 = 1
f' 1 = 1
f' n = fm (n - 1) + fm (n - 2)

memo :: Array Int Int
memo = listArray (lb, ub) [f' i | i <- [lb .. ub]]

fm :: Int -> Int
fm = (memo !)

-- * インタプリタパターン

-- |
-- EDSL で書かれた漸化式を Array operation に変換する
-- interpret :: EDSL -> Index -> Value
-- ただ、DP配列の数箇所をアクセスしたいみたいな需要もあるのでこの辺は再考の余地あり
-- 左辺部分が既に式になってる場合とかある: e.g. f (n + 1) とか このケースどうする？
newtype Eqn i a
  = Case [(Expr i a, Cond i a)]

data Cond i a
  = Otherwise
  | Const i
  | If (i -> Bool)

data Expr i a
  = Val a
  | Rec (i -> i)
  | UnOp (a -> a) (Expr i a)
  | BinOp (a -> a -> a) (Expr i a) (Expr i a)

(+#) :: Num a => Expr i a -> Expr i a -> Expr i a
(+#) = BinOp (+)

fd :: Eqn Int Int
fd =
  Case
    [ (Val 1, Const 0),
      (Val 1, Const 1),
      (Rec (\n -> n - 1) +# Rec (\n -> n - 2), Otherwise)
    ]

-- * 再帰版

-- |
-- interpret は引数のインデックス i を match で場合分けし、分岐先のケースを calc で計算する
interpret :: Eq i => Eqn i a -> i -> a
interpret eqn@(Case xs) i =
  case find (match i . snd) xs of
    Just aCase -> calc eqn i (fst aCase)
    Nothing -> error "unbound"

match :: Eq i => i -> Cond i a -> Bool
match i cond = case cond of
  Otherwise -> True
  Const j -> i == j
  If predicate -> predicate i

calc :: Eq i => Eqn i a -> i -> Expr i a -> a
calc eqn i expr = case expr of
  Val v -> v
  Rec r -> interpret eqn (r i)
  UnOp u e1 -> u (calc eqn i e1)
  BinOp b e1 e2 -> b (calc eqn i e1) (calc eqn i e2)

-- * 配列を使ったメモ化再帰

-- |
-- interpret はメモ化配列を生成して eval を発火させる
interpret' :: (Ix i, Enum i) => Eqn i a -> (i, i) -> i -> a
interpret' eqn (lb, ub) i =
  let arr = listArray (lb, ub) [eval eqn arr i' | i' <- [lb .. ub]]
   in eval eqn arr i

-- |
-- eval は引数のインデックス i を match で場合分けし、分岐先のケースを calc で計算する（先ほどの interpret）
eval :: Ix i => Eqn i a -> Array i a -> i -> a
eval (Case xs) arr i =
  case find (match i . snd) xs of
    Just aCase -> calc' arr i (fst aCase)
    Nothing -> error "unbound"

calc' :: Ix i => Array i a -> i -> Expr i a -> a
calc' arr i expr = case expr of
  Val v -> v
  Rec r -> arr ! r i
  UnOp u e1 -> u (calc' arr i e1)
  BinOp b e1 e2 -> b (calc' arr i e1) (calc' arr i e2)

-- ここまでの残り課題
-- 漸化式の左辺の添字が f n の形式に固定される（f (n + 1) とかできない）
-- いわゆる、貰うDPは書けるが、配るDPが書けない

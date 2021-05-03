# 動的計画法のフレームワーク化

目指す状態: 漸化式を再帰関数で書く→DPになる

最終形: [DPNkMassivM.hs](src/DPNkMassivM.hs)

利用例: [DPNkMassivMExamples.hs](src/DPNkMassivMExamples.hs): [AtCocer | D - Knapsack 1](https://atcoder.jp/contests/dp/submissions/22276490) に投げた

利用したテクニック
- n+k パターン： 漸化式の記法を柔軟にするため
- 配列操作のnewtype化: 操作の生成と操作の実行を分離するため
- 融合変換: 各インデックスに対する操作を配列を作らずに実行したかったので（Massivを利用）
  - (DP 配列を作らないという意味ではない)
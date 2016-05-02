■ Problem 23 「非過剰数和」
======================

■ リンク
--------------------

- <https://projecteuler.net/problem=23>
- <http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2023>

■ 問題
----------------------------

完全数とは, その数の真の約数の和がそれ自身と一致する数のことである.
たとえば, 28の真の約数の和は, 1 + 2 + 4 + 7 + 14 = 28 であるので, 28は完全数である.

真の約数の和がその数よりも少ないものを不足数といい, 真の約数の和がその数よりも大きいものを過剰数と呼ぶ.
12は, 1 + 2 + 3 + 4 + 6 = 16 となるので, 最小の過剰数である.

よって2つの過剰数の和で書ける最少の数は24である.
数学的な解析により, 28123より大きい任意の整数は2つの過剰数の和で書けることが知られている.
2つの過剰数の和で表せない最大の数がこの上限よりも小さいことは分かっているのだが, この上限を減らすことが出来ていない.

2つの過剰数の和で書き表せない正の整数の総和を求めよ.

■ 解答
----------------------

使用するモジュール

> import Data.List

2つの過剰数の和で書き表せない正の整数は,1以上28123以下。以下のステップで進める。

1. 過剰数をリストアップ
2. [1..28123]のリストに対して、過剰数リストの任意の要素の和で書けないものだけにフィルタリング
3. 和を算出

■ ステップ１：過剰数をリストアップ

過剰数をリストアップするために、ある数`x`が過剰数かどうかを判定する`isAbu x`を定義する。

> isAbu :: Int -> Bool

x/2, x/2-1, .. 1まで順に試し割りしていき、約数を探す。
見つけた約数を足し合わせていき、元の数より大きくなったら過剰数。

> isAbu x = exceed x (divs x) where
>   exceed x [] = False
>   exceed x [y]            = x < y
>   exceed x (y:ys) | x < y = True
>                   | x >=y = exceed x (y + head ys : tail ys)
>   divs x = filter (mod0 x) [halfOf x, halfOf  x - 1 .. 1]
>   halfOf x = x `div` 2
>   mod0 x y = x `mod` y == 0

`isAbu`を使って、ある数`x`より大きい最小の過剰数`nextAbu x`を定義する。

> nextAbuPoor :: Int -> Int
> nextAbuPoor x = (head . filter isAbu) [(x+1)..]

この`nextAbuPoor`だと毎回試し割りをしなければならないため、28123までリストアップするのに時間がかかる(O(N^2))。
過剰数の倍数は過剰数なので、すでに見つけた過剰数の倍数なら試し割りをスキップするように修正する。
`exs`は、すでに見つけた過剰数のリスト(ソート済み、重複なし)。

> nextAbu :: [Int] -> Int -> Int
> nextAbu exs x | x `elem` exs = nextAbu exs (x+1)
>               | isAbu x      = x
>               | otherwise    = nextAbu exs (x+1) where

`x`以下の過剰数のリスト`abus x`は、以下のように書ける。

> abus :: Int -> [Int]
> abus = abus' [] 12 where
>   abus' :: [Int] -> Int -> Int -> [Int]
>   abus' prev start limit
>     | start > limit = []
>     | otherwise     = multiples +++ abus' nextPrev nextNum limit where
>     multiples = [start, start*2..limit]
>     nextPrev = prev +++ multiples
>     nextNum = nextAbu nextPrev start

`(+++)`は、ソート済みで重複なしの二つのリストをソート済みで重複なしのリストに連結する。

>   (+++) x [] = x
>   (+++) [] x = x
>   (+++) (x:xs) (y:ys) | x<y  = x : (xs +++ (y:ys))
>                       | x>y  = y : ((x:xs) +++ ys)
>                       | x==y = x : (xs +++ ys)

■ ステップ２：[1..28123]のリストに対して、過剰数リストの任意の2要素の和で書けないものだけにフィルタリング

> nonAbuSums :: Int -> [Int]
> nonAbuSums x = filter isNonAbuSum [1..x]

以下のステップで判定する。

1. `x`未満の過剰数リストを取得。`theAbus`とする。
2. `x`に対して`theAbus`の要素をそれぞれ引いたリストを`subs`とする。
3. `subs`の要素のうち、`theAbus`に同じ値が含まれるものが1つでも存在すれば、`x`は2つの過剰数で書ける。

> isNonAbuSum :: Int -> Bool
> isNonAbuSum x = (never (`elem` theAbus) . map (x -)) theAbus where
>   theAbus = abus x
>   never x = not . any x

■ ステップ3：和を算出

> answer = sum $ nonAbuSums 28123

■ 計算

過剰数100ごとに途中結果を出力する。

> main :: IO ()
> main = do
>   mapM_ (print . last . abus) [100,200..28123]
>   print answer
>   print (last (abus 10000))

遅いのでもう少し早くする方法を考える必要あり。

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

2つの過剰数の和で書き表せない正の整数は,1以上28123以下。以下のステップで進める。

1. 28123以下の過剰数をリストアップ
2. 28123以下の任意の2つの過剰数の和をリストアップ
3. [1..28123]から過剰数和のリストの差集合を出し、和を算出

このファイルではステップ1だけ計算する。

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

> main :: IO ()
> main = mapM_ print $ abus 28123

過剰数リストを出力する。数分かかった。
`data/023-abus.txt`に出力するなら以下のとおり。

```bash
stack build :023-step1 ; stack exec 023-step1 > data/023-abus.txt
```

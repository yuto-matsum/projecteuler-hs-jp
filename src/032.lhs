■ Problem 32 「パンデジタル積」
=============================

■ リンク
----------

https://projecteuler.net/problem=32
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2032

■ 問題
-----------

すべての桁に 1 から n が一度だけ使われている数をn桁の数がパンデジタル (pandigital) であるということにしよう:
例えば5桁の数 15234 は1から5のパンデジタルである.

7254 は面白い性質を持っている.
39 × 186 = 7254 と書け, 掛けられる数, 掛ける数, 積が1から9のパンデジタルとなる.

掛けられる数/掛ける数/積が1から9のパンデジタルとなるような積の総和を求めよ.

HINT: いくつかの積は, 1通り以上の掛けられる数/掛ける数/積の組み合わせを持つが1回だけ数え上げよ.

■ 解答
---------------

使用するモジュールは以下

> import Data.Char (intToDigit,digitToInt)
> import Data.List (delete,sort,nub)

総当たりで掛けた時の計算量を調べる。

m桁*n桁の計算を(m,n)とすると、計算する桁は以下の20通りある。

(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),
      (2,2),(2,3),(2,4),(2,5),(2,6),(2,7),
            (3,3),(3,4),(3,5),(3,6),
                  (4,4),(4,5),

m桁*n桁=t桁の計算を(m,n,t)とすると、パンデジタル数となる組み合わせは以下のみに絞り込める。
他の組み合わせは積の桁数が合わない。

(1,4,4) (2,3,4) (3,3,3)

乗算の回数は、
(9*8*7*6*5)*2 + 9*8*7*6*5*4 = 90720
であり、それほど多くないので総当たりで計算してみる。

`n`個の数を`xs`から選んでつなげた数値を返す関数`iterateNumber`を定義する。

実行例：

-- iterateNumber [1..4] 3 == [123,124,132,134,142,143,213,214,231,234,241,243,312,314,321,324,341,342,412,413,421,423,431,432]

> iterateNumber :: [Int] -> Int -> [Int]
> iterateNumber xs n = map read $ permutation (map intToDigit xs) n

`iterateNumber`の定義のために、`n`個の要素からなる順列を列挙する関数`permutation`を定義。

> permutation :: Eq a => [a] -> Int -> [[a]]
> permutation _ 0  = []
> permutation xs 1 = [[x] | x<-xs]
> permutation xs n = concat [map (x:) (permutation (delete x xs) (n-1)) | x<-xs]

ここで、例えば2桁*3桁の乗算は、5つ選んで順列を作り、前半2つ掛ける後半3つと考えることができる。
m桁*n桁=t桁の計算を(m,n,t)とした場合、mとnの桁数を渡し、(m,n,t)を列挙する関数
`iterateMultiple`を定義する。（`iterateNumber`は使わなさそう。。。）

> iterateMultiple :: [Int] -> (Int,Int) -> [(Int,Int,Int)]
> iterateMultiple xs (m,n) = do
>   p <- permutation (map intToDigit xs) (m+n)
>   let numM = (read . take m) p :: Int
>       numN = (read . drop m) p :: Int
>       mul = numM * numN :: Int
>   return (numM, numN, mul)

なお、上記はリスト内包表記でもかけるが、煩雑になるのでdo構文で書いた。

[x*2 | x<-[0..9]]

は、以下のようにもかける。

```
do
  x<-[0..9]
  return x*2
```

(m,n,t)がパンデジタルか判定する関数を定義。

> isPan :: (Int,Int,Int) -> Bool
> isPan (m,n,t) = (map digitToInt . sort . concatMap show) [m,n,t] == [1..9]

(m,n,t)の桁数について、(1,4,4),(2,3,4),(3,3,3)で列挙し、パンデジタル数を列挙する。

> pans = (filter isPan . concatMap muls) [(1,4),(2,3),(3,3)] where
>   muls = iterateMultiple [1..9]

解答は、パンデジタル数の総和のため、積が重複するものを省く必要がある。
なので、積だけ取り出して、ソートして重複排除して和を取る。

> answer = (sum . nub . sort . map trd) pans where
>   trd (_,_,x) = x

> main = print answer

計算時間は2秒くらい。

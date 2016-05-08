■ Problem 27 「二次式素数」
==========================

■ リンク
-----------------------

https://projecteuler.net/problem=27
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2027

■ 問題
--------------

オイラーは以下の二次式を考案している:

```math
n^2 + n + 41.
```

この式は, n を0から39までの連続する整数としたときに40個の素数を生成する.
しかし, n = 40 のとき 402 + 40 + 41 = 40(40 + 1) + 41 となり41で割り切れる.
また, n = 41 のときは 412 + 41 + 41 であり明らかに41で割り切れる.

計算機を用いて, 二次式`n^2 - 79n + 1601`という式が発見できた.
これは n = 0 から 79 の連続する整数で80個の素数を生成する.
係数の積は, -79 × 1601 で -126479である.

さて, |a| < 1000, |b| < 1000 として以下の二次式を考える (ここで |a| は絶対値):
例えば |11| = 11 |-4| = 4である.

```math
n^2 + an + b
```

n = 0 から始めて連続する整数で素数を生成したときに最長の長さとなる上の二次式の, 係数 a, b の積を答えよ.

■ 解答
---------------

まず計算量についてざっと考える。
aとbはそれぞれ-1000から1000の2000通り。nの上限値はないが、問題文からせいぜい100未満ではないかと予想する。
平均でnは連続10程度と予想すると、 2,000 * 2,000 * 10 = 40,000,000 回の素数判定をする。
1回の素数判定が0.5秒の場合、愚直にやると約20,000,000秒=231日。

素数判定の数を減らすことと、素数判定の高速化を考えることを分けて実施する。

■ 判定回数の削減
----------------

まず、素数は2以上の自然数のため、n=0にて値が2未満となる(a,b)は省ける。
つまり、0^2+0a+b>=2 → b>=2

さらに、bは素数でなければならない。
なぜなら、n=0の時、値はbのため。

nの連続の最大は、明らかにb−1以下。
任意の(a,b)に対する素数判定回数の最大値はb−1。

y=n^2+an+bは、下に凸な二次方程式。
nを0から数十まで増やしていった時、値が2以上となる必要がある。
つまり、頂点が[0,100]にある場合、その値が2以上になる必要がある。
頂点のnは -a/2 であり、値域は以下のとおり。

```math
a^2 /4 - a * a/2 + b >= 2
b - a^2/4 >= 2
a^2 <= 4b-8
|a| <= √(4b-8)
```

書き直すと、

0 <= -a/2 <= 100 → -200 <= a <= 0 の場合、 -√(4b-8) <= a <= √(4b-8)
a>0の場合、特に他の制限はない。

つまり、bが決まったら、以下を満たすaのみ調べれば良い。

max(-200, -√(4b-8)) <= a <= 1000

まとめると、

1. bは素数のみ取りうる。
2. aは`max(-200, -√(4b-8)) <= a <= 1000`を満たす整数のみ取りうる。

■ 素数判定方法について
-------------------

素数判定の方法として分かり易く、利用しやすいのはエラトステネスのふるい。
この場合、調べたい素数は6桁程度になる場合があり、かつ飛び飛びの値を判定することになる。
他にはMiller-Rabin素数判定法や、Adleman-Pomerance-Rumely素数判定法がある。

Wikipedia > ミラー–ラビン素数判定法
https://ja.wikipedia.org/wiki/%E3%83%9F%E3%83%A9%E3%83%BC%E2%80%93%E3%83%A9%E3%83%93%E3%83%B3%E7%B4%A0%E6%95%B0%E5%88%A4%E5%AE%9A%E6%B3%95

Google検索 > adleman-pomerance-rumely test
https://www.google.co.jp/webhp?sourceid=chrome-instant&ion=1&espv=2&ie=UTF-8#q=adleman-pomerance-rumely+test

ひとまず、エラトステネスのふるいで200000までの素数をリストアップするのにかかる時間を計算してみる。
`src/027-primes1.hs`で実装する。

```sh
stack build :027-primes1 ; stack exec 027-primes1
17984
time: 1 sec.
```

2秒未満で出せたので、このロジックを使うことにする。

■ 実装
-----------------------

使用するモジュールは以下の通り

> import Data.Set (Set, empty, fromAscList, member, union)
> import Data.List (maximumBy)
> import Data.Function (on)

まず、素数リストを取得する。

> primes :: Int -> [Int] -- x: 素数の上限値
> primes x = primes' x empty 2 where
>   primes' :: Int -> Set Int -> Int -> [Int]
>   primes' limit ngs n -- 素数の上限値、今までに取得した合成数のリスト、現在の数値
>     | n > limit      = []
>     | n `member` ngs = primes' limit ngs (n+1)
>     | otherwise      = n : primes' limit nextNgs (n+1)
>     where nextNgs = ngs `union` fromAscList [n*2, n*3..limit]

> thePrimes = primes 200000

n^2 + a*n + bについて、n=0から素数が何個続くかを探索する関数`walk (a,b)`を定義。

> walk :: (Int,Int) -> Int
> walk (a,b) = w a b 0 where
>   w a b n | y `elem` thePrimes = 1 + w a b (n+1)
>           | otherwise          = 0
>     where y = n^2 + a*n + b

bを2から1000までの素数を大きい順で動かし、aをbに応じて`max(-200, -√(4b-8)) <= a <= 1000`の範囲で動かす。
その時の`((a,b),walk a b)`をリストアップする。

> solutions :: [((Int,Int),Int)]
> solutions = [((a,b), walk (a,b)) | b<-bs, a<-asFor b]
>   where
>     bs = (reverse . takeWhile (<=1000)) thePrimes
>     ceilroot :: Int -> Int
>     ceilroot = ceiling . sqrt . fromInteger . toInteger
>     minA :: Int -> Int
>     minA b = max (-200) (-1 * ceilroot (4*b-8))
>     asFor b = [minA b..1000]

> main :: IO ()
> main = print $ (calc . longest) solutions
>   where
>     longest = maximumBy (compare `on` snd)
>     calc = uncurry (*) . fst

計算時間は55秒くらい。

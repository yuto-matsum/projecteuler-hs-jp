Problem 3 「最大の素因数」
======================

リンク
-------------

- <https://projecteuler.net/problem=3>
- <http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%203>

問題
--------------------

13195 の素因数は 5, 7, 13, 29 である.
600851475143 の素因数のうち最大のものを求めよ.

解答
--------------------

> main = print answer

600851475143を素因数分解した時の、最後の要素が答え。

> answer = (last . factsOf) 600851475143

> factsOf 1 = []

素因数のリストを、最小の素因数と、残りのリストで分けて考える。

> factsOf x = first : remaining where

最小の素因数を探すために、[2,3,4,5,...,600851475143/2, 600851475143]で順に試し割りしていく。
余りが0のものが最小の素因数。

>   first = head [i | i <- [2..(x `div` 2)]++[x], x `mod` i == 0]

残りは、商に対して同様に試し割りしていく。

>   remaining = factsOf $ x `div` first

愚直だが計算量はO(NlogN)。

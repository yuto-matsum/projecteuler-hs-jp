{-
■ Problem 41 「パンデジタル素数」
===============================

■ 問題
---------------

n桁パンデジタルであるとは, 1からnまでの数を各桁に1つずつ持つこととする.
例えば2143は4桁パンデジタル数であり, かつ素数である.
n桁（この問題の定義では9桁以下）パンデジタルな素数の中で最大の数を答えよ.

■ 解答
----------------

sum [1..9]とsum [1..8]はそれぞれ45,36であり、ともに3の倍数。そのため9桁と8桁パンデジタル数は必ず3の倍数。
7桁の素数判定にエラトステネスのふるいを使った場合、どれくらい時間がかかるかを調べる。

`041-primes.hs`によると、7654321までの素数列を得るのに68秒くらいかかった。
まあ許容できる時間なので、これでいく。
7654321までの素数列を取得し、大きい順にパンデジタル数か判定する方針でいく。

--> 実行時間は65秒だった。
-}

import           Data.List (find, sort, sortBy)
import           Data.Set  (Set, empty, fromAscList, member, union)

main :: IO ()
main = print $ (find isPan . sortDesc . primes) 7654321

primes :: Int -> [Int]
primes x = primes' x empty 2 where
  primes' :: Int -> Set Int -> Int -> [Int]
  primes' limit ngs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > limit      = []
    | n `member` ngs = primes' limit ngs (n+1)
    | otherwise      = n : primes' limit nextNgs (n+1)
    where nextNgs = ngs `union` fromAscList [n*2, n*3..limit]

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

isPan :: Int -> Bool
isPan x = let s = show x in sort s == concatMap show [1..length s]

{-
■ Problem 41 「パンデジタル素数」
エラトステネスのふるいで7654321の素数判定にかかる時間を測定する。
以下で時間を出力できる。
stack build :041-primes; time stack exec 041-primes

ghciで時間を調べるには以下のコマンドを実行すれば良い。
:set +s

実行時間は70秒くらいだった。
-}

import           Data.Set (Set, empty, fromAscList, member, union)

main :: IO ()
main = (print . length . primes) 7654321

-- エラトステネスのふるいによる素数列の取得
primes :: Int -> [Int] -- 素数の上限値
primes x = primes' x empty 2 where
  primes' :: Int -> Set Int -> Int -> [Int]
  primes' limit ngs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > limit      = []
    | n `member` ngs = primes' limit ngs (n+1)
    | otherwise      = n : primes' limit nextNgs (n+1)
    where nextNgs = ngs `union` fromAscList [n*2, n*3..limit]

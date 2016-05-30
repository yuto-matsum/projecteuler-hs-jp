{-
Problem 69 「トーティエント関数の最大値」 †
オイラーのトーティエント関数, φ(n) [時々ファイ関数とも呼ばれる]は, n と互いに素な n 未満の数の数を定める. たとえば, 1, 2, 4, 5, 7, そして8はみな9未満で9と互いに素であり, φ(9)=6.

n	互いに素な数	φ(n)	n/φ(n)
2	1	1	2
3	1,2	2	1.5
4	1,3	2	2
5	1,2,3,4	4	1.25
6	1,5	2	3
7	1,2,3,4,5,6	6	1.1666...
8	1,3,5,7	4	2
9	1,2,4,5,7,8	6	1.5
10	1,3,7,9	4	2.5
n ≤ 10 では n/φ(n) の最大値は n=6 であることがわかる.

n ≤ 1,000,000で n/φ(n) が最大となる値を見つけよ.


解答
--------

"値"ってnなのかn/phi(n)なのか解りにくい。英語を見るとnの方っぽい。

素数pのφ(p)はp-1と大きく、n/φ(n)は低いので無視。

数nの約数が多いほど、φ(n)が小さくなる。

 * pを素数、kを自然数とした時、 φ(p^k)= p^(k-1) * (p-1)
 * mとnが互いに素である場合、 φ(m*n) = φ(m) * φ(n)

=> 数nを素因数分解し,素数ごとにφを出して乗算する？

φ(1024)   = φ(2^10) = 2^9 * (1) = 512
φ(1024*3) = φ(2^10) * φ(3) = 2^9 * 1 * 3^0 * 2 = 512 * 2

1024と2048のrateは同じ。素因数の種類が増えないとrateは大きくならない。
また、n/phi(n)は素数が小さいほど大きくなる。

素因数の種類が多く、かつ素数自体が小さい方が良い。
つまり、2*3*5*...と乗算を繰り返して、100万以下での最大値が答え？

-}

import           Data.Function      (on)
import           Data.List          (maximumBy, nub)
import           System.Environment (getArgs)


main = do
  txt <- readFile "data/047-primeFactors.txt"
  let pfs = (take 1000000 . map (map read . words) . lines) txt
  print $ answer pfs

answer :: [[Int]] -> Int
answer = product . maximumBy (compare `on` (length . nub))


-- main = do
--   rs <- getArgs
--   let n= (read . head) rs
--   print $ answer n

-- answer :: Int -> Int
-- answer n = maximumBy (compare `on` rate) [2..n]

rate :: Int -> Double
rate x = fromIntegral x / fromIntegral (phi x)

phi :: Int -> Int
phi x = (length . filter (relPri x)) [1..(x-1)]

relPri :: Int -> Int -> Bool
relPri x = (==1) . gcd x

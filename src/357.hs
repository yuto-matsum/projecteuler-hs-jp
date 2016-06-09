{-
Problem 357 「素数生成整数」
===============================

30の約数について考えよう : 1,2,3,5,6,10,15,30.
30の約数 d は, そのすべてにおいて d+30/d の値が素数になる.

n のすべての約数 d について d+n/d が素数になるような, 100 000 000 以下の正の整数 n の合計を求めよ.

解答
-------------

1は必ず約数に入る。そのため素数より1小さい数が候補。1以外は偶数になる。

1億以下の素数は何個あるか？
問題41でエラトステネスのふるいで出したら、700万くらいが70秒だった。1億は10分以上かかるかも。。。

30: 1,  2,  3,  5,  6, 10, 15, 30
-> 31, 17, 13, 11, 11, 13, 17, 31
               ->| ここまで調べれば十分。
整数nの約数の半分は、n/4以下？ --> NO
試しに平方根をとってみた。√30=5.47

120でも検算。√120=10.95

     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  16
120: 1, 2, 3, 4, 5, 6, 8,10,12,15,20,30,40,60,80,120
                         ->|
あってるっぽい。

1億以下の偶数nについて、以下の手順で調べる。

1. n+1した数が素数かミラーラビンテストで調べる。素数なら2に進む。違うなら次の偶数へ。
2. 2+(n/2)が素数かミラーラビンテストで調べる。素数なら3に進む。違うなら次の偶数へ。
3. nの約数を出す。半分の約数までで出せば十分なので、√n以下まで。
4. 約数dについてd+(n/d)が素数か調べる。全て素数なら該当。1つでも違えば次の偶数へ。

--> N=10k以下で1.2秒弱。100kで13秒なのでほぼ線形。
100Mは100kの1k倍なので、13k秒=3.6時間。
エラトステネスのふるいでやってみる。

--> ダメ。もっと時間が掛かった。357i.hs

--> ミラーラビンで実行したら、五分弱で出た。
時間がかかってたのはGHCiだったためだったようで。
-}

main = print $ answer 100000000

answer = sum . primeGens

-- m以下の素数生成整数を全て取得
primeGens :: Int -> [Int]
primeGens m = 1: (filter areAllPrimes . filter (isPrimeD 2) . filter isPrimeD1) [2,4..m] where
  isPrimeD1 = isPrimeByMR 3 . (+1)
  isPrimeD d = isPrimeByMR 3 . numD d
  numD d = (+d) . (`div` d)
  areAllPrimes x = all (`isPrimeD` x) (divs x)
  divs x = filter (\y -> x `mod` y == 0) [1..rt x]
  rt = truncate . sqrt . fromIntegral

{- べき剰余
>>> pow 1234 5678 9 == fromIntegral (1234 ^ 5678 `mod` 9)
True
-}
pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow b x m | odd  x = b * pow b (x-1) m `mod` m
          | even x = pow b (x`div`2) m ^2 `mod` m

-- 一つでもTrueがあればFalseを返却
never :: (a->Bool) -> [a] -> Bool
never f = not . any f

-- 底が2の対数の整数部分
log2 :: Int -> Int
log2 = truncate . logBase 2.0 . fromIntegral

-- ミラー・ラビンテスト ------------------------------------

{-
N回のミラー・ラビンテストによって、数xが素数か判定
xが奇数の場合、N個の証人に対して一度も合成数と判定しなかったら素数と判断
>>> isPrimeByMR 3 531
False
>>> isPrimeByMR 3 65537
True
-}
isPrimeByMR :: Int -> Int -> Bool
isPrimeByMR n x
  | x < 2  = False
  | even x = x==2
  | odd x  = never isMultiple wits where
  wits = (take n . mrWits) x
  isMultiple = milrab x

-- ミラー・ラビンテストを用いて、奇数xが合成数か証人aを用いて判定
milrab :: Int -> Int -> Bool
milrab x a = (x-1) `notElem` pow2s where
  (s,d) = splitForMR x
  pow2s = (take s . iterate pow2 . pow a d) x
  pow2 i = pow i 2 x

-- 素数判定したい奇数xに対して x-1 == 2^s*d を満たす自然数sと奇数dを取得
splitForMR :: Int -> (Int,Int)
splitForMR x = divBy2Pow 1 ((x-1) `div` 2) where
  divBy2Pow s d | odd d     = (s,d)
                | otherwise = divBy2Pow (s+1) (d `div` 2)

-- ミラー・ラビンテストにおける、数xの証人に使える数のリスト
-- w^d `mod` x /= 1を満たす数w
mrWits :: Int -> [Int]
mrWits x = [w | w<-[2..(x-1)], pow w d x /= 1] where
  d = (snd . splitForMR) x

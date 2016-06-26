{-
Problem 387 「ハーシャッド数」
=============================

ハーシャッド数　(Harshad Number), あるいはニーベン数 (Niven Number) とは自身の各桁の和で割り切ることのできる数のことである.
201 は (自身の各桁の和である) 3 で割り切ることができるのでハーシャッド数である.
201 の最後の桁を切り詰めると 20 が得られ, これはハーシャッド数である.
20 の最後の桁を切り詰めると 2 が得られ, これもまたハーシャッド数である.
ハーシャッド数の最後の桁を再帰的に切り詰めていってもハーシャッド数となるものを右切り詰め可能ハーシャッド数 (right truncatable Harshad number) と呼ぼう.

同様に:
201/3=67 は素数である.
その自身の各桁の和で割ると素数になるハーシャッド数を強いハーシャッド数 (strong Harshad number) と呼ぼう.

ここで素数 2011 を見てみよう.
最後の桁を切り詰めると 201 となり, これは強いハーシャッド数であるとともに右切り詰め可能である.
このような素数を強い右切り詰め可能ハーシャッド素数 (strong, right truncatable Harshad primes) と呼ぼう.

10000 未満の強い右切り詰め可能ハーシャッド素数の和は 90619 となる.

10^14 未満の強い右切り詰め可能ハーシャッド素数の和を求めよ.

解答
--------------

定義が引っかかりやすく混乱したのでまとめる。

強い(st)：その数を桁の和で割ったら素数になる
右切り詰め可能(re)：再帰的にハーシャッド数
強い右切り詰め可能ハーシャッド素数(strepr)：素数であり、右を切り詰めると強い右切り詰めハーシャッド数になる

つまり、streprは
- ハーシャッド数ではない。単に素数。
- 再帰的に強くなくて良い。1つ右切り詰めした数が強ければ良い。

手順として、以下を試みる。

1. 10^13以下の右切り詰め可能ハーシャッド数をリスティング: recHars
    1. 1桁のハーシャッド数をリスティング: h1
    2. h1の右に[0..9]を追加し、ハーシャッド数だけにフィルタリング: h2
    3. h13まで繰り返し
    4. [h1..h13]をconcat
2. recHarsのうち、強いハーシャッド数だけにフィルタリング: strRecHars
3. strRecHarsの右に数[0..9]をそれぞれ追加してconcatし、素数だけにフィルタリング: strRecHarPris
4. 和を取る: answer

--> 10000未満では検算が通るのに、10^14未満だと答えが合わない。d=10以降で各リストを調べてみると以下。

| d | #recHarsOf | #recHars | #strRecHars | #strRecHarPris |
|---|------------|----------|-------------|----------------|
|  9|       1001 |     2724 |          88 |             42 |
| 10|       1386 |     4110 |         112 |             45 |
| 11|       2019 |     6129 |         130 |             45 |
| 12|       2760 |     8889 |         130 |             45 |
| 13|       3864 |    12753 |         130 |             45 |
| 14|       5156 |    17909 |         130 |             45 |

12桁以降の強い右切り詰め可能ハーシャッド数が見つからない。
素数判定がダメなのか？MRテストの証人を10まで増やしてみる。

--> 関係ないみたい

-}

import           Data.Char (digitToInt)

main = print $ answer 14

-- 答えは10^d未満の強い右切り詰め可能ハーシャッド素数の和
answer :: Int -> Int
answer = sum . strRecHarPris

-- 10^d未満の強い右切り詰め可能ハーシャッド素数のリスト
strRecHarPris :: Int -> [Int]
strRecHarPris = filter isPrime . concatDigit . strRecHars . sub1

sub1 x = x - 1

-- 10^d未満の強い右切り詰め可能ハーシャッド数のリスト
strRecHars :: Int -> [Int]
strRecHars = filter isStrong . recHars

-- 10^d未満の右切り詰め可能ハーシャッド数をリスティング
recHars :: Int -> [Int]
recHars d = concatMap recHarsOf [1..d]

-- d桁の右切り詰め可能ハーシャッド数をリスティング
recHarsOf :: Int -> [Int]
recHarsOf 1 = [1..9]
recHarsOf d = (filter isHarshad . concatDigit . recHarsOf . sub1) d

-- それぞれの右に[0..9]を追加する
concatDigit :: [Int] -> [Int]
concatDigit = concatMap addDigit

-- xの右側に1桁追加する
addDigit :: Int -> [Int]
addDigit x = [x*10+d | d<-[0..9]]

-- nがハーシャッド数か判定
isHarshad :: Int -> Bool
isHarshad = (==0) . snd . tupleForHarshad

-- nを各桁の和で割った時の商と余り
tupleForHarshad :: Int -> (Int,Int)
tupleForHarshad n = (divMod n . sum . map digitToInt . show) n

-- nが強い(ハーシャッド)数か判定
isStrong :: Int -> Bool
isStrong = isPrime . fst . tupleForHarshad

-- xが素数か判定
isPrime :: Int -> Bool
isPrime = isPrimeByMR 3

-- ミラー・ラビンテスト >>>>>>>>>>>

{- べき剰余　pythonのpowメソッドのクローン
>>> pow 1234 5678 9 == fromIntegral (1234 ^ 5678 `mod` 9)
True
-}
pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow b x m | odd  x = b * pow b (x-1) m `mod` m
          | even x = pow b (x`div`2) m ^2 `mod` m

-- N回のミラー・ラビンテストによって、数xが素数か判定
isPrimeByMR :: Int -> Int -> Bool
isPrimeByMR n x
  | x < 2  = False
  | even x = x==2
  -- N個の証人に対して、一度も合成数と判定しなかったら素数と判断
  | odd x  = never isMultiple wits where
  wits = (take n . mrWits) x
  isMultiple = milrab x

-- 一つでもTrueがあればFalseを返却
never :: (a->Bool) -> [a] -> Bool
never f = not . any f

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

-- 底が2の対数の整数部分
log2 :: Int -> Int
log2 = truncate . logBase 2.0 . fromIntegral

-- ミラー・ラビンテスト <<<<<<<<<<<<<<<<<<<

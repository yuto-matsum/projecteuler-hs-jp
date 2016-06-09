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

n桁の強い右切り詰め可能ハーシャッド素数は、(n-1)桁の右切り詰め可能ハーシャッド数を含む。

1. 1桁のハーシャッド数をリストアップ：h1
2. 2桁の右切り詰め可能ハーシャッド数をh1からリストアップ:h2
3. h14まで順にリストアップ
4. h1...h14のうち、素数だけをフィルタリング
5. 和を取る

-}
import           Data.Char (digitToInt)

main = print $ answer 14

answer :: Int -> Int
answer = sum . harshads

harshads :: Int -> [Int]
harshads 0 = [1..9]
harshads d = undefined
-- harshads d = map (map (++[0-9]) . show) harshads (d-1)

isHarshad :: Int -> Bool
isHarshad n = ((n `mod0`) . sum . map digitToInt . show) n

mod0 :: Int -> Int -> Bool
mod0 x y = x `mod` y == 0

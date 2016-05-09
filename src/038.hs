{-
■ Problem 38 「パンデジタル倍数」
==================================

■ リンク
------------------------

https://projecteuler.net/problem=38
http://odz.sakura.ne.jp/projecteuler/index.php?cmd=read&page=Problem%2038

■ 問題
--------------------------
192 に 1, 2, 3 を掛けてみよう.

192 × 1 = 192
192 × 2 = 384
192 × 3 = 576

積を連結することで1から9の パンデジタル数 192384576 が得られる. 192384576 を 192 と (1,2,3) の連結積と呼ぶ.

同じようにして, 9 を 1,2,3,4,5 と掛け連結することでパンデジタル数 918273645 が得られる. これは 9 と (1,2,3,4,5) との連結積である.

整数と (1,2,...,n) (n > 1) との連結積として得られる9桁のパンデジタル数の中で最大のものはいくつか?
-}

{-
■ 解答
----------------------------

方針：

例で書かれている通り、9桁目が9のパンデジタル数918273645が存在する。
そして明らかに最大のパンデジタル数は987654321だが、連結積は存在しない(n>=1なら存在するが)。
918273645 < p < 987654321となるパンデジタル数を大きい方から探索し、その数が連結積としてかけるか判定する。

連結積でかけるかどうかの判定は、初めの積が1桁の場合、2桁の場合、...4桁の場合というように試していく。
例えば、987654321について、nを動かして積と実際の数字を比べる。

* 1桁の場合: n=1:    9, n=2:    9*2=   18 /=   87
* 2桁の場合: n=1:   98, n=2:   98*2=  196 /=  765
* 3桁の場合: n=1:  987, n=2:  987*2= 1974 /= 6543
* 4桁の場合: n=1: 9876, n=2: 9876*2=19752 /=54321

初めの積が5桁の時、n=2で明らかに4桁より大きくなるため、初めの積は高々4桁まで。
-}

import           Data.Char (intToDigit)
import           Data.List (delete, nub, sort, (\\))

-- 解答は連結積で表せるパンデジタル数の最大
answer = maximum conMuls
-- 連結積で表せるパンデジタル数
conMuls = filter conMul trimPans
-- パンデジタル数の最大は918273645より大きく987654321未満(文字列のまま比較可能)
trimPans = filter (range "918273645" "987654321") pans
-- 文字列に変換したパンデジタル数 ["912..", "921..", ...]
pans = map (map (intToDigit . fromInteger)) panNums
-- 9で始まるパンデジタル数を全て列挙 [[9,1,2..],[9,2,1..], ...]
panNums = (map (9:) . permutation) [1..8]

-- 指定された範囲に、値が含まれているか判定。始点と終点は含まない。
range :: Ord a => a -> a -> a -> Bool
range mn mx n = mn < n && n < mx

{-
順列を列挙

>>>permutation "abc"
["abc","acb","bac","bca","cab","cba"]
>>>permutation "wow"
["oww","wow","wwo"]
-}
permutation :: Ord a => [a] -> [[a]]
permutation [] = []
permutation [x] = [[x]]
permutation xs = (nub . sort . concat) [map (x:) (permutation (delete x xs)) |x<-xs]

{-
連結積があるか判定。
初めの積が1桁から4桁までの場合でそれぞれ判定し、見つかった時点でTrueを返して終了。

>>> conMul "192384576"
True
>>> conMul "987654321"
False
-}
conMul :: String -> Bool
conMul x = any (conMulByN1Digit x) [1..4]

{-
n=1の積がd桁の場合、連結積が作れるか判定

>>> conMulByN1Digit "192384576" 3
True
>>> conMulByN1Digit "192384576" 2
False
-}
conMulByN1Digit :: String -> Int -> Bool
conMulByN1Digit x d = (x `startsWith` hd) && nextConMul hdN tl sc 2 where
  hdN = (read . take d) x :: Int    -- hd: n=1の積
  hd = show hdN           :: String
  sc = (show . (*2)) hdN  :: String -- sc: n=2の積を表す文字列
  tl = drop d x           :: String -- tl: n=2以降の数を表す文字列

{-
conMulByN1Digitで使用する内部関数

>>> nextConMul 192 "384576" "384" 2
True
>>> nextConMul 192 "576" "576" 3
True
>>> nextConMul 98 "7654321" "196" 2
False
-}
nextConMul :: Int -> String -> String -> Int -> Bool
nextConMul top x hd n =
  (x==hd) || ((x `startsWith` hd) && nextConMul top tl sc (n+1))
  where
    tl = x \\ hd                   -- n+1以降で残っている数字
    sc = (show . (*top) . (+1)) n  -- n+1における正解の積

{-
>>>startsWith "abcef" "abc"
True
>>>startsWith "hogeFugaPiyo" "Fuga"
False
-}
startsWith :: Eq a => [a] -> [a] -> Bool
startsWith _ [] = True
startsWith [] _ = False
startsWith (x:xs) (y:ys) = (x==y) && startsWith xs ys

-- 時間は50秒くらい
main = print answer

{-
■ Problem 46 「もうひとつのゴールドバッハの予想」
=============================================

問題
-----------

Christian Goldbachは全ての奇合成数は平方数の2倍と素数の和で表せると予想した.

9 = 7 + 2×1^2
15 = 7 + 2×2^2
21 = 3 + 2×3^2
25 = 7 + 2×3^2
27 = 19 + 2×2^2
33 = 31 + 2×1^2

後に, この予想は誤りであることが分かった.

平方数の2倍と素数の和で表せない最小の奇合成数はいくつか?


解答
------------------

答えが何桁か予想がつかないため、ひとまず100万まで次の手順で探索することにする。

1. N=1000000とする。
2. 2以上N未満の素数をリストアップ: pris
3. 2以上N未満の平方数の2倍をリストアップ: dsqs
4. 9以上N以下の奇合成数をリストアップ: odds

5. oddsを小さい方から判定: o
  1. prisを小さい順に(p<o)まで取得: p
  2. (o-p)がdsqsに含まれるか判定。含まれるならoは外れ、次のoへ。
  3. oに対して、(o-p)がdsqsに含まれるpが一つもないなら、それが答え。

--> 100万では応答が返ってこない。100位から順に増やしていくことにする。
--> Nが増えた時、奇合成数のリストアップに時間がかかる。Setを使ってみる。
--> N=10000の時、45秒で答えが出た。

-}

import           Data.Set (Set, empty, filter, findMin, fromAscList, insert,
                           member, split, union)
import           Prelude  hiding (filter)
-- import           System.Environment (getArgs)

type I=Int -- Integerにするかもしれないので別名にしておく

-- main = do
--  args<-getArgs
--  let n = (read . head) args
--  putStrLn $ "N: " ++ show n
--  putStrLn $ "last pris: " ++ (show . findMax . pris) n
--  putStrLn $ "last dsqs: " ++ (show . findMax . dsqs) n
--  putStrLn $ "last odds: " ++ (show . findMax . odds) n
--  putStrLn $ "answer: " ++ (show . answer) n

main = print $ answer 10000

answer :: I -> Maybe I
answer x = searchOdds x (odds x)

-- x以下の素数と平方数の2倍の和で書けない奇合成数を探す
searchOdds :: I -> Set I -> Maybe I
searchOdds x ys
  | searchCmb x h = searchOdds x ((snd . split h) ys)
  | otherwise     = Just h where
  h = findMin ys

-- 数oがx以下の素数と平方数の2倍の和でかけるか判定
searchCmb :: I -> I -> Bool
searchCmb x o = any (\p -> (o-p) `member` dsqs x) ((fst . split o . pris) x)

-- エラトステネスのふるいによる素数列の取得
pris :: I -> Set I
pris x = pris' x empty 2 where
  pris' :: I -> Set I -> I -> Set I
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = empty
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = n `insert` pris' s xsNx (n+1)
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

-- 平方数の2倍のリスト
dsqs :: I -> Set I
dsqs x = fromAscList [2*i*i | i<-[1..(hf . rt) x]] where
  hf = (`div` 2)
  rt = truncate . sqrt . fromIntegral

-- 奇数の合成数のリスト
odds :: I -> Set I
odds x = (filter (`notElem` pris x) . filter odd . fromAscList) [9..x]

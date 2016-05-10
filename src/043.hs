{-
■ Problem 43 「部分文字列被整除性」
================================

■ 問題
----------

数1406357289は0から9のパンデジタル数である (0から9が1度ずつ現れるので). この数は部分文字列が面白い性質を持っている.

d1を上位1桁目, d2を上位2桁目の数とし, 以下順にdnを定義する. この記法を用いると次のことが分かる.

d2d3d4=406 は 2 で割り切れる
d3d4d5=063 は 3 で割り切れる
d4d5d6=635 は 5 で割り切れる
d5d6d7=357 は 7 で割り切れる
d6d7d8=572 は 11 で割り切れる
d7d8d9=728 は 13 で割り切れる
d8d9d10=289 は 17 で割り切れる
このような性質をもつ0から9のパンデジタル数の総和を求めよ.

■ 解答

まず計算量を調べる。
[0..9]のパンデジタル数は10!-9!=3,265,920とおり。

愚直に列挙して、そのまま一つづつ調べてみることにする。

フィルタは、絞り込みが強くて判定が楽なものから順にかけていく。

--> 4秒くらいで答えが出る。
遅延評価なので、フィルタリングが通らない数はどんどん省かれるためだろう。
-}

import           Data.Char (digitToInt, intToDigit)
import           Data.List (delete)

main = print answer
answer = (sum . map read) mulPans

mulPans :: [String]
mulPans = filters [mul5,mul2,mul17,mul13,mul11,mul7,mul3] pans

-- 倍数のフィルタで順に絞り込んでいく。
filters :: [a->Bool] -> [a] -> [a]
filters fs xs = foldl (flip filter) xs fs

-- 2の倍数: d4が[0,2..8]のいずれか
mul2 = (`elem` mul2valids) . (!!3)
mul2valids = ['0','2','4','6','8']

-- 3の倍数: d3+d4+d5が3の倍数、[3,6..24]のいずれか
mul3 = (`elem` mul3valids) . sum . map digitToInt . take 3 . drop 2
mul3valids = [3,6..24]

-- 5の倍数: d6が[0,5]のいずれか
mul5 = (`elem` mul5valids) . (!!5)
mul5valids = ['0','5']

-- 7の倍数: d5d6d7が[7,14..987]のいずれか(候補は104個)
mul7 = (`elem` mul7valids) . take 3 . drop 4
mul7valids = (filter notSames . map to3digit) [7,14..987]

-- 11の倍数: d6d7d8が[11,22..987]のいずれか(候補は64個)
mul11 = (`elem` mul11valids) . take 3 . drop 5
mul11valids = (filter notSames . map to3digit) [11,22..987]

-- 13の倍数: d7d8d9が[13,26..987]のいずれか(候補は57個)
mul13 = (`elem` mul13valids) . take 3 . drop 6
mul13valids = (filter notSames . map to3digit) [13,26..987]

-- 17の倍数: d8d9d10が[17,34..987]のいずれか(候補は44個)
mul17 = (`elem` mul17valids) . drop 7
mul17valids = (filter notSames . map to3digit) [17,34..987]

{-
3桁以下の数値を長さ3の文字列にする
>>> to3digit 7
"007"
-}
to3digit :: Int -> String
to3digit x
  | x<10 = "00" ++ show x
  | x<100 = "0" ++ show x
  | x<1000 = show x

-- 文字列が全て違う文字でできているか判定
notSames :: String -> Bool
notSames [] = True
notSames (x:xs) = x `notElem` xs && notSames xs

-- 0から9までのパンデジタル数を列挙する
pans :: [String]
pans = (toStr . filter not0start . perms) [0..9]

-- 順列を列挙
perms :: Eq a => [a] ->[[a]]
perms [] = []
perms [x] = [[x]]
perms xs = concat [map (x:) (perms (delete x xs)) | x<-xs]

-- 9桁目が0でないか判定
not0start :: [Int] -> Bool
not0start = (/=0) . head

-- >>> [[1,2,3],[4,5,6]]
-- ["123","456"]
toStr = map (map intToDigit)

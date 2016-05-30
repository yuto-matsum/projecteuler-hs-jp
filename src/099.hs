{-
Problem 99 「最大のべき乗」
==========================

指数の形で表される2つの数, 例えば 2^11 と 3^7, の大小を調べることは難しくはない.
電卓を使えば, 2^11 = 2048 < 3^7 = 2187 であることが確かめられる.

しかし, 632382^518061 > 519432^525806 を確認することは非常に難しい
(両者ともに300万桁以上になる).

各行に1組が書かれている1000個の組を含んだ22Kのテキストファイル base_exp.txt から,
最大の数が書かれている行の番号を求めよ.

注: ファイル中の最初の二行は上の例である.

解答
=======

大小を比較するために、両辺をX倍したりY乗したりしてみる。

   2^11 , 3^7
=> 2^7 * 2^4 , 3^7
=> (2/3)^7 * 2^4, 1
=> 2^4, (3/2)^7

底と指数は減ったが、あまり速くならなさそう。
遅延評価とHaskellの力を信じて愚直にやってみる。
--> 4分で出た。

-}
import           Data.Function (on)
import           Data.List     (elemIndex, maximumBy)
import           Data.Maybe    (fromJust)

-- カンマでリスト化
wordsByComma :: String -> [String]
wordsByComma x
  | w == x = [x]
  | otherwise = w : wordsByComma (drop (length w + 1) x) where
  w = takeWhile (/=',') x

-- 愚直にやる案 ------------------------------------
main = do
  txt <- readFile "data/p099_base_exp.txt"
  let nums = (map (map read . wordsByComma) . lines) txt
      maxExp = maximumBy (compare `on` (\[x,y]->x^y)) nums
  print maxExp
  print $ ((+1) . fromJust . elemIndex maxExp) nums

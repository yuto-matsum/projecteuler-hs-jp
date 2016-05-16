{-
Problem 50 「連続する素数の和」
===========================

問題
------------

素数41は6つの連続する素数の和として表せる:

41 = 2 + 3 + 5 + 7 + 11 + 13.

100未満の素数を連続する素数の和で表したときにこれが最長になる.

同様に, 連続する素数の和で1000未満の素数を表したときに最長になるのは953で21項を持つ.

100万未満の素数を連続する素数の和で表したときに最長になるのはどの素数か?

解答
-----------------

この問題を解くために、あるリストxsの部分リストについて、和がそのリストに出てくる部分リストの最長を調べる。

100万未満の素数なら、エラトステネスのふるいを使って求め手もそれほど時間はかからない。
素数リストをpsとする。

S(=1000000)未満の素数リストの先頭から、以下を繰り返す。
R(=[1..1000])の各要素rについて、
要素数rの部分リストの和が、S未満の素数リストに含まれる部分リストを探す

-->S=1000000, R=[1..1000]について、3.5時間走らせたら出た。

改善点
-----------------

以下の点で改善できる。

1.探索方向
2.和の計算

大きい順に探索し、見つかった時点で終了すれば、かなりの時間短縮になる。
つまりR=[1000,999..1]を、mapではなくfindで探索する。

部分リストの和は、普通にsumで計算している。
しかし、前の部分リストに1回和算、1回減算すれば出せる。

0番目から始まるN個の部分リストの和sum_0 は、sum (take n ps)
1番目から始まるN個の部分リストの和sum_1 は、sum0 - (ps!!0) + (ps!!(n))
2番目から始まるN個の部分リストの和sum_2 は、sum1 - (ps!!1) + (ps!!(n+1))
i番目から始まるN個の部分リストの和sum_i: sum_(i-1) - (ps!!(i-1)) + (ps!!(n+i-1))

-}

import           Data.List  (delete, find, nub, sort, (\\))
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.Set   (Set, empty, fromAscList, member, union)

-- ((連続する数, 連続する素数の和), (連続する素数の最小, 最大))
main = mapM_ (print . (\ x -> ((length x, sum x), (head x, last x)))) (mapMaybe (findSeq thePris) searchRange)

thePris = pris 1000000
searchRange = [1..1000]

findSeq :: [Int] -> Int -> Maybe [Int]
findSeq xs n = find (\x->sum x `member` xset) [(take n . drop i) xs| i<-[0..(length xs - n)]]
  where
    xset = fromAscList xs

-- エラトステネスのふるいによる素数列の取得
pris :: Int -> [Int]
pris x = pris' x empty 2 where
  pris' :: Int -> Set Int -> Int -> [Int]
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = []
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = n : pris' s xsNx (n+1)
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

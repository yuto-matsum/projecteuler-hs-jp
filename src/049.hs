{-
Problem 49 「素数数列」
================================

問題
-------------

項差3330の等差数列1487, 4817, 8147は次の2つの変わった性質を持つ.

(i)3つの項はそれぞれ素数である.
(ii)各項は他の項の置換で表される.

1, 2, 3桁の素数にはこのような性質を持った数列は存在しないが, 4桁の増加列にはもう1つ存在する.

それではこの数列の3つの項を連結した12桁の数を求めよ.

解答
-----------------

以下のステップで。

1. 4桁の素数の一覧取得。
2. 素数リストから、各項の置換で書ける組を一覧取得。
3. 各組から3個選び、等差数列になるものを探す。

-->1秒以下で出た。
-}

import           Data.List  (delete, find, nub, sort, (\\))
import           Data.Maybe (mapMaybe)
import           Data.Set   (Set, empty, fromAscList, member, union)

main = print answer

answer :: [String]
answer = (map (concatMap show) . mapMaybe arithmetic3) permPris

-- 受け取った数を使って3項の等差数列を作る
arithmetic3 :: [Int] -> Maybe [Int]
arithmetic3 xs = find isArithmetic3 (choice 3 xs)
  where isArithmetic3 x = head x + (x!!2) == (x!!1) * 2

-- リストからN個を選択する組み合わせを列挙
choice :: Int -> [a] -> [[a]]
choice n (x:xs)
  | n == 1   = map (:[]) (x:xs)
  | n <  len = map (x:) (choice (n-1) xs) ++ choice n xs
  | n == len = [x:xs]
  | n >  len = []
  where len = length (x:xs)

-- 置換できる素数のリストを取得
permPris :: [[Int]]
permPris = permPris' pri4digits where
  permPris' [] = []
  permPris' (p:ps) = pp : permPris' (ps \\ pp) where
    pp = ((p:) . filter (`elem` ps) . intPerms) p

-- 4桁の素数
pri4digits :: [Int]
pri4digits = dropWhile (<1000) (pris 9999)

-- エラトステネスのふるいによる素数列の取得
pris :: Int -> [Int]
pris x = pris' x empty 2 where
  pris' :: Int -> Set Int -> Int -> [Int]
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = []
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = n : pris' s xsNx (n+1)
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

{-
置換した文字を取得
>>> intPerms 1230
[1023,1032,1203,1230,1302,1320,2013,2031,2103,2130,2301,2310,3012,3021,3102,3120,3201,3210]
-}
intPerms = nub . sort . map read . filter ((/='0') . head) . perms . show

-- 順列を列挙
perms :: Eq a => [a] ->[[a]]
perms [] = []
perms [x] = [[x]]
perms xs = concat [map (x:) (perms (delete x xs)) | x<-xs]

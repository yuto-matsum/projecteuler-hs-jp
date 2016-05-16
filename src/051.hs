{-
Problem 51 「素数の桁置換」
===========================

問題
---------

*3の第1桁を置き換えることで, 13, 23, 43, 53, 73, 83という6つの素数が得られる.

56**3の第3桁と第4桁を同じ数で置き換えることを考えよう.
この5桁の数は7つの素数をもつ最初の例である: 56003, 56113, 56333, 56443, 56663, 56773, 56993.
よって, この族の最初の数である56003は, このような性質を持つ最小の素数である.

桁を同じ数で置き換えることで8つの素数が得られる最小の素数を求めよ. (注:連続した桁でなくても良い)

解答
------------

問題文がふわっとしててよくわからない。

ひとまず以下のように解いてみる。

1. 固定桁の素数リストを取得。5桁でひとまずやってみて、ダメなら6桁以降も探索する。
2. 素数を小さい順に次のステップを実施:
    1. 0を[1..9]へ置換した組、..8を[9]へ置換した組を列挙
    2. 列挙した組内の数値を、素数リストに含まれるものだけにフィルタリング
    3. 長さが最大の組を取得
    4. その組の7以上、自身の素数を含めて8以上あるなら、その素数が答え。なければ次の素数を調べる。

--> 9秒で出た。

-}

import           Data.Char     (intToDigit)
import           Data.Function (on)
import           Data.List     (delete, find, maximumBy, nub, sort, (\\))
import           Data.Maybe    (catMaybes, fromJust, isJust, mapMaybe)
import           Data.Set      (Set, empty, fromAscList, insert, member, split,
                                toAscList, union)

-- 5桁以上の素数で、8個以上の置換した素数の組を探す。
main = print $ (fromJust . fromJust . find isJust) (map (answer 8) [5..])

-- n個以上の置換した素数の組を、d桁の素数から探す
answer :: Int -> Int -> Maybe [Int]
answer n d = find (\x -> length x >= n) (eachRepPris d)

-- d桁の素数の中から、置換した素数の組になっている組み合わせを列挙
eachRepPris :: Int -> [[Int]]
eachRepPris d = map (\x->x: iterRepPris ps x) (toAscList ps)
  where ps = prisOf d

-- d桁の素数リスト
prisOf d = (snd . split (10^(d-1)) . pris) (10^d-1)

-- 素数pの桁を置換した素数の組み合わせを列挙。ただしp自身より大きいものに限る。
iterRepPris :: Set Int -> Int -> [Int]
iterRepPris ps = maximumBy (compare `on` length) . map (filter (`member` ps)) . iterRepInts

-- 名前がいまいちなのでリネーム
toChar = intToDigit

-- ある数xの桁を置換した数字の組み合わせを、桁ごとに列挙
iterRepInts :: Int -> [[Int]]
iterRepInts x = map (iterRepInt (show x)) [0..9]

-- ある数xsにある数字Nを、[N+1..9]に置換した数値を列挙。ただしxsより大きい数に限る。
iterRepInt :: String -> Int -> [Int]
iterRepInt [] n = []
iterRepInt xs n
  | c `elem` xs = map ((read . replace xs c) . toChar) [(n+1)..9]
  | otherwise   = []
  where c = toChar n

-- xsにあるyを全てzに変更
replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = []
replace (x:xs) y z = (if x==y then z else x) : replace xs y z

-- エラトステネスのふるいによる素数列の取得
pris :: Int -> Set Int
pris x = pris' x empty 2 where
  pris' :: Int -> Set Int -> Int -> Set Int
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = empty
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = insert n (pris' s xsNx (n+1))
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

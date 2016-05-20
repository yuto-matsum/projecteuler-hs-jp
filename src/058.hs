{-
Problem 58 「螺旋素数」 †
========================

問題
-----------

1から始めて, 以下のように反時計回りに数字を並べていくと, 辺の長さが7の渦巻きが形成される.

37	36	35	34	33	32	31
38	17	16	15	14	13	30
39	18	05	04	03	12	29
40	19	06	01	02	11	28
41	20	07	08	09	10	27
42	21	22	23	24	25	26
43	44	45	46	47	48	49

面白いことに, 奇平方数が右下の対角線上に出現する.
もっと面白いことには, 対角線上の13個の数字のうち, 8個が素数である.
ここで割合は8/13 ≈ 62%である.

渦巻きに新しい層を付け加えよう. すると辺の長さが9の渦巻きが出来る.
以下, この操作を繰り返していく.
対角線上の素数の割合が10%未満に落ちる最初の辺の長さを求めよ.

解答
-----------

辺の長さ3の時、外周の数値は[2..9],対角線の数値は[3,5,7,9]
辺の長さ5の時、外周の数値は[10..25],対角線の数値は[13,17,21,25]
辺の長さ7の時、外周の数値は[26..49],対角線の数値は[31,37,43,49]

辺の長さが2i+1の時:
* 外周の数値は[((2i-1)^2+1)..(2i+1)^2]
* 対角線の数値は[(2i+1)^2-6i,(2i+1)^2-4i,(2i+1)^2-2i,(2i+1)^2]

探索する辺の長さの上限を決めて調べる。

--> 素数の割合が50%~14%くらいまでの層数をプロットしてみたところ
10%未満になるのは3000層〜4000層くらいになりそう。
3000層は、素数リストが6000^2=36000000まで必要になり、かなり時間が掛かる。
そうなるとメモリも7GBくらい食ってしまうため、改善が必要。

改善案
--------------

素数判定する数は、層数*3個。4000層なら12000個程度。
リスト全体は8000^2くらいあるので、1/6000くらいしかない。

エラトステネスのふるいのために素数リストを用意するのではなく、
フェルマーテストやミラー・ラビンテストで素数判定をしてみる。

--> 058-improved.hsにて

-}

import           Data.List          (find)
import           Data.Maybe         (catMaybes, fromMaybe, isJust)
import           Data.Set           (Set, empty, fromAscList, insert, member,
                                     size, toAscList, union)
import           System.Environment (getArgs)

main = do
  rs <- getArgs
  let n= (read . head) rs
  putStrLn $ "limit: " ++ show n
  putStrLn $ "primes: [2 .. " ++ show ((n*2-1)^2) ++ "]"
  putStrLn $ "prime size: " ++ (show . size . pris) ((n*2-1)^2)
--   putStrLn "writing primes..."
--   writeFile "data/058-primes.txt" ((unlines . map show . toAscList . pris) ((n*2-1)^2))
--   putStrLn "done."
  mapM_ print $ mapWhile isJust (answer n) [1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10]

-- 条件を満たすまでmap
mapWhile :: (b->Bool) -> (a->b) -> [a] -> [b]
mapWhile b f xs
  | null xs  || (not . b . f . head) xs = []
  | otherwise = (f . head) xs : mapWhile b f (tail xs)

-- d層(辺の長さは2d-1)まで螺旋素数を調べ、素数の割合がr未満になる辺を調べる。
answer :: Int -> Double -> Maybe (Int, Int)
answer d r = (find (\x-> r > rate x) . tail . take d) (rounds d)


rounds d = iterate (nextRound (pris n)) (1,0) where
  n = (d*2-1)^2 - (d*2-1)+1

-- (辺の長さ,対角線上の素数の個数)から、素数の割合を返却
rate :: (Int,Int) -> Double
rate (x,y) = fromIntegral y / fromIntegral (crossLength x)

-- (辺の長さ, 対角線上の素数の個数)を受け取り、次の層を追加した時のそれを返却
-- nextRound (5,5) = (7,8)
nextRound :: Set Int -> (Int,Int) -> (Int,Int)
nextRound ps (x,y) = (x+2, y+delta) where
  delta = (length . filter (`member` ps)) cans
  cans = [s-3*d, s-2*d, s-d] where
    s = (x+2)^2
    d = x+1

-- 辺の長さxにおける対角線の長さ
crossLength :: Int -> Int
crossLength x = 2*x-1

-- エラトステネスのふるいによる素数列の取得
pris :: Int -> Set Int
pris x = pris' x empty 2 where
  pris' :: Int -> Set Int -> Int -> Set Int
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = empty
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = insert n (pris' s xsNx (n+1))
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

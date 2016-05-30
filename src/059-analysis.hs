{-
Problem 59 「XOR暗号解読」
================================

cipher1.txtを3個のリストに分配し、数の頻度をそれぞれ解析してみる。
以下のような感じで。

1,2,3,4,5,6,1,2,3
-> [1,4,1],[2,5,2],[3,6,3]
-> [(1,2),(4,1)],[(2,2),(5,1)],[(3,2),(6,1)]
-}
import           Data.Bits       (xor)
import           Data.Char       (chr, ord)
import           Data.List       (sort, sortOn)
import           Data.List.Split (splitEvery, splitOn)

解析用
main = do
  txt <- readFile "data/p059_cipher.txt"
  let ws = (dist3 . splitOn ",") txt
  mapM_ (print . count) ws

{-
>>> dist3 [1,2,3,4,5,6,7]
[[1,4,7],[2,5],[3,6]]
-}
dist3 :: [a] -> [[a]]
dist3 xs = [s0,s1,s2] where
  s0 = [xs!!i | i<-[0,3..length xs -1]]
  s1 = [xs!!i | i<-[1,4..length xs -1]]
  s2 = [xs!!i | i<-[2,5..length xs -1]]

{-
>>> count [1,5,1,4,4,2,4]
[(4,3),(1,2),(2,1),(5,1)]
-}
count :: Ord a => [a] -> [(a,Int)]
count = reverse . sortOn snd . count' . (`zip` repeat 1) . sort where
  count' :: Ord a => [(a,Int)] -> [(a,Int)]
  count' []  = []
  count' [x] = [x]
  count' ((x,i):(y,j):zs) | x==y      = count' ((x,i+j):zs)
                          | otherwise = (x,i): count' ((y,j):zs)

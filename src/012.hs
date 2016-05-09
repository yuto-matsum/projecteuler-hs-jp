import           Data.List
import           Data.Maybe
-- import           MyLib

-- answer = find divis500over $ ntail 1000 tris
--   where
--     divis500over x = divisCount x > 500

-- 三角数xの素因数分解をする
-- [(p,n)]=素数p,pの数n
-- [(2,3),(7,2)]の場合, 約数の数は...
-- (0,0)個選ぶ,(0,1),(0,2), (1,0),(1,1),(1,2),
-- (2,0),(2,1),(2,2), (3,0),(3,1),(3,2)
-- == 4*3 == 12個
-- == (3+1) * (2+1) == nの積

-- 500 = 5*5*5*2*2
-- 2^4*3^4*5^4*7*2*11*2

-- 約数の個数
divisCount 1=1
divisCount x = foldl1 (*) $ map (+1) $ countDup $ fact x

dc' 1 = 1
dc' x | even x    = divisCount(div x 2) * divisCount(x+1)
      | otherwise = divisCount(x) * divisCount(div (x+1) 2)

-- dc'(T1) = 1
-- dc'(T2) = f(1) * f(3)
-- dc'(T3) = dc'(T2) * dc3
-- dc'(T4) = dc'(T3) }

main = print $ answer 500
answer n = find (dc'over n) [1..]
  where dc'over thr x = dc' x > thr

-- dc'72 = dc'36*dc73
--       = dc'18*dc37,73
--       = dc'9*dc19,37,73
--       = dc'5*dc9,19,37,73
--       = dc'3*dc5,9,19,37,73
--       = dc'2*dc3,5,9,19,37,73
--       = dc'1*dc3,3,5,9,19,37,73
--       = 1     *2*2*2*3 *2 *2 *2 ==
-- 2^3*3*2 = 12: 1,2,3,4,6,8,9,12,18,24,36,72

countDup [] = []
countDup xs = count (xs!!0) xs : (countDup $ delFromAsc (xs!!0) xs)


{-
>>> fact 12
[2,2,3]
-}
fact :: Integral a => a -> [a]
fact 1 = []
fact i = first i : remain i
  where
    first :: Integral a => a -> a
    first i = [2..] !! firstIdx i
    firstIdx :: Integral a => a -> Int
    firstIdx i = fromJust $ findIndex (mod0 i) [2..]
    mod0 :: Integral a => a -> a -> Bool
    mod0 i j = mod i j == 0
    remain :: Integral a => a -> [a]
    remain 1 = []
    remain i = fact $ div i $ first i
count :: (Eq a,Integral b) => a -> [a] -> b
count _ [] = 0
count i (j:js) | i==j      = 1+count i js
               | otherwise = count i js

-- 昇順に並んでいるリストの先頭からxを削除する
delFromAsc _ [] = []
delFromAsc x (y:ys) | x==y      = delFromAsc x ys
                    | otherwise = y:ys

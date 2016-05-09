
-- i=4, facts i=[[],[],[2],[3],[2,2]],
--      pows i=[[0,0..], [0,0..], [0,0,1,0,0..],
--              [0,0,0,1,0,0,..], [0,0,2,0,0..]]
--      max(pows i)=[0,0,2,1,0,0..]
--      max(take i (pows i))=[0,0,2,1,0]

import           Data.List  (findIndex, maximum, transpose)
import           Data.Maybe (fromJust)
main = undefined
answer :: Int -> Int
answer n = [0..n] ^* maxpows n

(^*) :: Integral a => [a] -> [a] -> a
(^*) [] _ = 1
(^*) _ [] = 1
(^*) (i:is) (j:js) = (i^j) * (is^*js)

maxpows :: Int -> [Int]
maxpows i = maxs $ map (take (i+1)) $ pows i

-- maxs [[0,1],[2,1],[0,3]]=[2,3]
maxs :: Ord a => [[a]] -> [a]
maxs [] =  []
maxs xss = map maximum $ transpose xss

pows :: Int -> [[Int]]
pows i = map pow (facts i)
  where
    pow :: [Int] -> [Int]
    pow [] = [0,0..]
    pow xs = [count i xs | i<-[0..]]

facts :: Integral a => Int -> [[a]]
facts i = map fact $ take i [1..]

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

count :: Eq a => a -> [a] -> Int
count _ [] = 0
count i (j:js) | i==j      = 1+count i js
               | otherwise = count i js

--------------------------------------
-- import Data.List (find)
-- import Data.Maybe (isNothing)
-- answer :: Int -> Int
-- answer n = foldl1 (*) $ filter' notInclude [2..n]
--
-- filter' :: ([a]->Bool)->[a]->[a]
-- filter' op [] = []
-- filter' op is | op is   = (is!!0):filter' op (tail is)
--               | otherwise = filter' op (tail is)
-- notInclude :: Integral a => [a] -> Bool
-- notInclude is = if length is > 0
--   then isNothing $ find (mod0 (is!!0)) (tail is)
--   else False
-- mod0 :: Integral a => a -> a -> Bool
-- mod0 i j = mod j i == 0
--------------------------------------
-- import Data.List (find,findIndex,intersect)
-- import Data.Maybe (fromJust,isNothing)

-- answer :: Int -> Int
-- answer n = map (*) $ concat $ minPrimeSet $ facts n

-- facts :: Int -> [[Int]]
-- facts i = map fact $ take (i-1) ints
--   where
--     ints = [2,3..]
--     fact :: Int -> [Int]
--     fact 1 = []
--     fact i = first i : remain i
--
--     first :: Int -> Int
--     first i = ints !! firstIdx i
--     firstIdx :: Int -> Int
--     firstIdx i = fromJust $ findIndex (mod0 i) ints
--     mod0 :: Int -> Int -> Bool
--     mod0 i j = mod i j == 0
--
--     remain :: Int -> [Int]
--     remain 1 = []
--     remain i = fact $ div i $ first i

-- FIXME intersect is not work
-- minPrimeSet :: [[Int]]->[[Int]]
-- minPrimeSet xs = filter (nubList 0) xs
--   where
--     nubList i is    = isNothing $ findInTail i is
--     findInTail i is = find (include is!!i) (ntail i is)
--     include i j     = intersect i j == i
--     ntail i is      = [(is!!j) | j<-[(i+1)..(length is-1)]]

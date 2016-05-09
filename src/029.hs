import Data.List (sort)
import MyLib (fact)

-- a=2
-- b=100

facts :: Integral a => a -> a -> [[a]]
facts a b = map fact [a..b]
pow :: Integral a => Int -> [a] -> [a]
pow n xs = (sort.concat.(take n).repeat) xs

uniq :: Eq a => [a] -> [a]
uniq []     = []
uniq [x]    = [x]
uniq (x:xs) = if x == head xs then uniq xs else uniq xs ++ [x]

powfacts :: Int -> Int -> [[Int]]
powfacts a b = (uniq.sort.concat) [map (pow n) (facts a b)|n<-[a..b]]
answer :: Int -> Int -> Int
answer a b  = length (powfacts a b)

main = print (answer 2 100)

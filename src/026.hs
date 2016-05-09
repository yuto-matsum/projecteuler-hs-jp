import Data.List (maximumBy)
import MyLib (fact,numOfDgt)

main = print $ answer 1000

answer n = sndmax (cycs n)

-- sndmax [(2,0),(0,5),(3,1)]==(0,5)
-- sndmax [(2,0),(0,2),(3,3)]==(3,3)
sndmax :: Ord a => [(b,a)] -> (b,a)
sndmax = maximumBy (\(x,y) (z,w)->compare y w)

-- cycs 3 == []
cycs :: Integral a => a -> [(a,a)]
cycs n = zip [1..n] [cyc i | i<-[1..n]]

-- trim 6==3, trim 70=7
trim :: Integral a => a -> a
trim n = (product.drop25.fact) n

-- drop25 [2,2,3,3,5,5,5,7] == [3,3,7]
drop25 :: Integral a => [a] -> [a]
drop25 [] = []
drop25 (x:xs) = if x==2 || x==5
                then drop25 xs else x:drop25 xs

-- cyc 10 == 0, cyc 3 == 1, cyc 7 == 6
cyc :: Integral a => a->a
cyc n | trim n == 1 = 0
      | otherwise   = helper 1 where
  helper i | isCyc n i = i
           | otherwise = helper (i+1)

-- isCyc 3 1 == True, isCyc 7 6 == True
isCyc :: Integral a => a -> a -> Bool
isCyc n d= f==g where
  f = sub (trim n) (d+offset)
  g = sub (trim n) (d*2+offset) `mod` (10^d)
  offset = numOfDgt (trim n) - 1

-- sub 6 2 == 16, sub 6 5 == 16666
sub :: Integral a => a -> a -> a
sub n i = numer `div` demin where
  numer = 10^(fromIntegral i)
  demin = fromIntegral n


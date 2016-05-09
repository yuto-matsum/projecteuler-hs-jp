import System.Environment
import MyLib (primes,numOfDgt)

-- main = print $ answer 1000000
main = do
       args <- getArgs 
       let n = read (args!!0) :: Integer
       print $ answer n

-- binary search by sorted list
elemBin :: Ord a => a -> [a] -> Bool
elemBin x xs = helper x xs 0 (length xs -1) where
  helper x xs low high
     | high < low  = False
     | xs!!mid > x = helper x xs low (mid-1)
     | xs!!mid < x = helper x xs (mid+1) high
     | otherwise   = True where
       mid = low + ((high - low) `div` 2)

-- circle number
-- cic 1234 = [1234,2341,3412,4123]
cic :: Integral a => a-> [a]
cic n = cicHelper n d where
  d   = numOfDgt n
  cicHelper n 1 = [n]
  cicHelper n x = n : cicHelper (nxt n) (x-1)
  nxt x = lst*(10^(d-1))+(exl`div`10) where
    lst = x `mod` 10
    exl = x - lst

-- If xs is the subset of ys, return true.
cont :: Ord a => [a] -> [a] -> Bool
cont xs ys = all (==True) [elemBin x ys | x<-xs]

cicPs :: Integer -> [Integer]
cicPs n = filter (\p->cont (cic p) ps) ps where
  ps = takeWhile (<n) primes

answer n = length $ cicPs n


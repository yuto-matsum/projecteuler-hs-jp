-- (220、284)、(1,184、1,210)、(2,620、2,924)、(5,020、5,564)、(6,232、6,368)、
--
import           System.Environment

amicable :: Int -> Int -> Bool
amicable n m
  | sum(propDivis n) /= m = False
  | otherwise             = sum(propDivis m) == n

amicables :: Int -> [[Int]]
amicables n = [[i,j]|i<-[4..n],j<-[i..min n (i*16`div`10)],amicable i j]

propDivis :: Int -> [Int]
propDivis n = [i | i<-[1..(n-1)], n `mod` i == 0]

answer' n = concat [ps | ps<-amicables n]
answer n = concat [if p/=q then p:q:[] else [] | (p:q:[])<-amicables n]

main = do
  args <- getArgs
  let n = read (head args)
  print $ answer' n
  print $ sum $ answer' n
  print $ answer n
  print $ sum $ answer n

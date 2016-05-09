import MyLib ((++^))
import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isP10 :: Int -> Bool
isP10 n = let s = show n
  in s == reverse s

dgtToBin :: Int -> String
dgtToBin n = showIntAtBase 2 intToDigit n ""

isP2 :: Int -> Bool
isP2 n = let s = dgtToBin n
  in s == reverse s

pal10s = filter isP10 [1..]
pal2s  = filter isP2  [1..]

answer n = sum (t10 ++^ t2) where
  t10= takeWhile (<n) pal10s
  t2 = takeWhile (<n) pal2s

main = print $ answer 1000000


isP10 n | n<10                 = True
        | n<100                = n`mod`11 == True
        | mid < (keta`div`100) = mid==0
        | otherwise            = where
  keta = 10
  mid = (n`mod`keta)`div`10


-- 11023
--  102


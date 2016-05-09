import MyLib(primes,elemBin,numOfDgt,uniq)
import Data.List(permutations,subsequences,sort)
import System.Environment

p = permutations
s = subsequences

cs::[String]
cs = (drop 1 . concat . map p . s) "337799"

cs'::[Integer]
cs' = map read cs

ps::[Integer]
ps = takeWhile (<1000000) primes

ps'::[Integer]
ps'=filter (\x->elemBin x ps) cs'

cs2::[Integer]
cs2 = (map read . p) "337799"

ps2rl::[Integer]
ps2rl=filter helper cs2 where
  helper x | x>=10 && existsPs x = helper (x`div`10)
           | existsPs x == False = False
           | otherwise           = True
  existsPs x = elemBin x ps

-- 739397

rtol::Integer -> [Integer]
rtol n =if n>=10 then n : rtol (n`div`10)
                 else [n]

ltor::Integer -> [Integer]
ltor n =if n>=10 then n : ltor (n`mod`(10^(numOfDgt n-1)))
                 else [n]

rounds::Integer -> [Integer]
rounds n = (uniq.reverse.sort) (rtol n ++ ltor n)

allPrime :: Integer -> Bool
allPrime n = all (\x->elemBin x (takeWhile (<=n) primes)) (rounds n)


--  37 73
--     797 
--        3797 
-- 739397
main = do
       args <- getArgs
       let n = read $ head args
       let ps = takeWhile (<=n) primes
       print $ find allPrime ps

-- :main 739397 = [2,3,5,7,23,37,53,73,313,317,373,797,3137,3797,739397]
--
answer = sum [23,37,53,73,313,317,373,797,3137,3797,739397]
-- 748317


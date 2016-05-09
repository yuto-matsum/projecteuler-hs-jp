--import           MyLib
main=undefined

answer n = sum $ digits n
  where
  --n = pow 100


{-| split by each digit number

>>> digits 123
[3,2,1]

>>> digits 24381
[1,8,3,4,2]
-}
digits :: Integral a => a -> [a]
digits n = [(n `div` (10^(i-1))) `mod` 10 | i<-[1..(numOfDgt n)]]


{-| number of digits

>>> numOfDgt 1234
4

>>> numOfDgt 34567
5

>>> numOfDgt (10^12)
13
-}
numOfDgt :: Integral a => a -> a
numOfDgt n | div n 10 /=0 = 1 + numOfDgt (div n 10)
           | otherwise    = 1

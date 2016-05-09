import           Data.List (find, maximumBy, sortBy)

main = undefined
-- answer = find isPal3 answers

-- isPal3 (_,_,x) = isPal x

answers :: [(Integer,Integer,Integer)]
answers = sortBy od3 $ map mul $ cmb [999,998..100]
  where mul (x,y) = (x,y,x*y)
        od3 (_,_,x) (_,_,y) | x > y     = LT
                            | x < y     = GT
                            | otherwise = EQ

-- isPal :: Integer -> Bool
-- isPal x = if length s > 2 then f==l && isPal s
--           else            f==l
--   where f = head s
--         l = last s
--         s = show x


-- tod :: (Integer,Integer) -> (Integer,Integer) -> Ordering
-- tod (x,y) (z,w) | x*y > z*w  = LT
--                 | x*y < z*w  = GT
--                 | otherwise  = EQ

-- answer = sortBy tod3 (filter isPal3 answers)
-- tod3 (_,_,x) (_,_,y) | x>y = LT
--                      | x<y = GT
--                      | otherwise = EQ

-- isPal 9009 = True
-- isPal :: Integer -> Bool
-- isPal x | 0 <=x && x<=9  = True
--         | 10<=x && x<=99 = x==0 || mod x 11 == 0
--         | otherwise      = lstN == fstN && nxtPal
--           where
--             fstN = mod x 10
--             lstN = div x (dgn x)
--             nxtPal = if nxtN >= dgn x `div` 100
--                      then isPal nxtN else nxtN == 0
--             nxtN = (x - (fstN+lstN*(dgn x))) `div` 10

-- dgn 3423 = 1000
-- dgn :: Integer -> Integer
-- dgn x = let k = truncate . logBase 10 . fromIntegral
--        in 10 ^ k x

-- cmb [1..3] =
cmb []     = []
cmb [x]    = [(x, x)]
cmb (x:xs) = ((x, x) : [(x, y) | y <- xs]) ++ (cmb xs)


main = print $ answer 6

answer n = foldl1 (*) [d (10^i)|i<-[0..n]]

-- d 12==1
d :: Integer -> Integer
d n = keta m i where
  m = number n
  i = index n

-- keta 3456 0 == 6, keta 3456 2 == 4
keta :: Integer -> Integer -> Integer
keta n i = (n `div` (10^i)) `mod` 10 

-- number 5 == 5,
-- number 10 == 10, number 11 == 10, number 12 == 11
number :: Integer -> Integer
number n | n< 10 = n
         | n>=10 = (10 ^ order n) + (offset `div` len) where
  offset = n - baseidx n
  len = order n + 1

-- n=5, offset=5-0, len=1, number=10^0 + 5/1=1

-- index {0-9} == 0,
-- index 10 == 1, index 11 == 0,
-- index 12 == 1, index 13 == 0
-- index 190 == 2, index 191 == 1, index 192 == 0
index :: Integer -> Integer
index n = order n - (offset `mod` len) where
  offset = n - baseidx n
  len = order n + 1

-- order {0-9} == 0, order {10-189} == 1
order :: Integer -> Integer
order n = helper 1 where
  helper i | n < idx i = i-1
           | otherwise = helper (i+1)

-- baseidx {0-9} == 0, baseidx {10-189} == 10
baseidx :: Integer -> Integer
baseidx n = helper 0 where
  helper i | idx i <=n && n<idx (i+1) = idx i
           | otherwise = helper (i+1)

-- idx 0 == 0, idx 1 == 190
idx i | i<=0 = 0
      | i==1 = 10
      | i> 1 = idx (i-1) + i*((10^i)-(10^(i-1)))


import           Data.List (findIndex)

main=undefined
-- combination nCr
comb :: Integral a=>a->a->a
comb n r | d>0      = (s n (r-d))`div`(s (r-d) (r-d-1))
         | otherwise= (s n r)    `div`(s r (r-1)) where
  s = subpower
  d = 2*r-n

-- i * i-1 * ... * i-j+i
subpower :: Integral a=>a->a->a
subpower i j = product [i,i-1..i-j+1]

-- max: comb n (n+1)`div`2

-- Data.List.findIndex (>1000000) [comb i ((i+1)`div`2)|i<-[1..]]
-- Just 22
answer n = (length. filter (>1000000)) [comb i j|i<-[23..n],j<-[1..i]]

main = print $ answer 100
-- 4075

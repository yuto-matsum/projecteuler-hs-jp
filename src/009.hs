import           Data.List

main=undefined
isPt x y z | x+y <= z     = False
           | x^2+y^2==z^2 = True
           | otherwise    = False

-- answer = find matched (pts 500)
--   where matched (x,y,z) = x+y+z==1000 && isPt x y z
--         pts n = [(i,j,k) | i<-[1..n], j<-[i..n], k<-[j..n]]

answer = find matched (pts 500)
  where matched (x,y,z) = isPt x y z
        pts n = [(i,j,1000-i-j) | i<-[1..n], j<-[i..n]]

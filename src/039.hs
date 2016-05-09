import           Data.List          (sortBy)
import           MyLib              (isInt)
import           System.Environment

main=undefined
--a^2+b^2=c^2
--p=a+b+c

istri90 (a,b,c) = a^2+b^2==c^2

cands n=[(i,j,n-i-j)|i<-[1..n'],j<-[i..n'']] where
  n' =(n`div`3)
  n''=(n`div`2)

tri90s n=filter istri90 (cands n)

answer n = [(i,length (tri90s i),tri90s i)|i<-[n`div`2..n]]

main = do
       as <- getArgs
       let n = read $ head as
       print $ sortBy (\(_,x,_)(_,y,_)->compare x y) (answer n)

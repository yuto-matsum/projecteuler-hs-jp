import           Data.List          (find)
import           MyLib              (qsort)
import           System.Environment
import           System.Environment

main=undefined
samed :: Integer -> Bool
samed x = all (== m 1) [m i|i<-[2..6]] where
  m n = (qsort.show) (x*n)

answer n = find samed [1..n]
answer'  = find samed [1..]

main = do
       args <- getArgs
       if length args > 0 then (print.answer.read.head) args
                          else print answer'

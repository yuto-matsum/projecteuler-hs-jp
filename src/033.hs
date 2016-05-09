import           Data.Ratio ((%))
main=undefined

isCrs (i,j,k) | i==j&&j==k  = False
              | otherwise=(10*i+j)%(10*j+k)==i%k

crss = filter isCrs [(i,j,k)|i<-[1..9],j<-[1..9],k<-[1..9]]

rat (i,j,k) = i%k

answer = map rat crss
answer' = product answer

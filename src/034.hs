

pow n = foldl (*) 1 [2..n]

p = [pow i | i<-[0..9]]

a n = if n<10 then p!!n else p!!(n`mod`10) + a (n`div`10)

isSame n = n==a n

answer = filter (\x->isSame x) [i | i<-[3..3000000]]

main = do
  print answer
  print $ sum answer




p :: Int -> [Int]
p n = [i^n | i<-[0..9]]

a :: Int -> Int -> Int
a n m = if n<10 then (p m) !! n
                else ((p m) !! (n`mod`10)) + a (n`div`10) m

isSame :: Int -> Int -> Bool
isSame n m = n == a n m

answer = filter (\x-> isSame x 5) [i|i<-[395245,395244..2]]

main = do
  print answer
  print $ sum answer


main = print answer
answer = sum $ filter even $ take maxN fibs
maxN = 32
fibs = 1:2:(zipWith (+) fibs $ tail fibs)
-- map ((-) 4000000) $ take 33 fibs

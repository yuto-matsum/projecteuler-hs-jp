main=undefined
answer = length $ combinations sums

coins = [200,100,50,20,10,5,2,1]
sums  = [200,0,0,0,0,0,0,0]

combinations :: [Int] -> [[Int]]
combinations ts
  | next ts == [] = [ts]
  | otherwise     = ts : combinations(next ts)

next :: [Int] -> [Int]
next ts
  | ts!!6>0        = fst' 6 ++ [                  (ts!!6)-2,   (ts!!7)+2]
  | ts!!5>0&&mod2  = fst' 5 ++ [ts!!5-  5, sum(snd' 6)+   5,           0]
  | ts!!5>0        = fst' 5 ++ [ts!!5-  5, sum(snd' 6)+   4,           1]
  | ts!!4>0        = fst' 4 ++ [ts!!4- 10, sum (snd' 5)+ 10,         0,0]
  | ts!!3>0        = fst' 3 ++ [ts!!3- 20, sum (snd' 4)+ 20,       0,0,0]
  | ts!!2>0&&mod20 = fst' 2 ++ [ts!!2- 50, sum (snd' 3)+ 50,     0,0,0,0]
  | ts!!2>0        = fst' 2 ++ [ts!!2- 50, sum (snd' 3)+ 40,    10,0,0,0]
  | ts!!1>0        = fst' 1 ++ [ts!!1-100, sum (snd' 2)+100,   0,0,0,0,0]
  | ts!!0>0        =           [0,                      200, 0,0,0,0,0,0]
  | otherwise      = [] where
    fst' n  = fst $ splitAt n ts
    snd' n  = snd $ splitAt n ts
    mod2    = (sum(snd' 6)+ 5) `mod`  2 == 0
    mod20   = (sum(snd' 3)+50) `mod` 20 == 0

-- down' :: [Int] -> [Int]
-- down' ts
--   | ts!!6>0                          = fst' 6 ++ [(ts!!6)-2, (ts!!7)+2]
--   | take 2 (snd' 5)==[5,0]           = fst' 5 ++ [0, sum (snd' 5), 0]
--   | ts!!5>0                          = fst' 5 ++ [ts!!5-5, ts!!6+4, ts!!7+1]
--   | take 3 (snd' 4)==[10,0,0]        = fst' 4 ++ [0, sum (snd' 4), 0, 0]
--   | ts!!4>0                          = fst' 4 ++ [ts!!4-10, ts!!5+10] ++ snd' 6
--   | take 4 (snd' 3)==[20,0,0,0]      = fst' 3 ++ [0, sum (snd' 3), 0, 0, 0]
--   | ts!!3>0                          = fst' 3 ++ [ts!!3-20, ts!!4+20] ++ snd' 5
--   | take 5 (snd' 2)==[50,0,0,0,0]    = fst' 2 ++ [0, sum (snd' 2), 0, 0, 0, 0]
--   | ts!!2>0                          = fst' 2 ++ [ts!!2-50, ts!!3+40, ts!!4+10]++ snd' 5
--   | take 6 (snd' 1)==[100,0,0,0,0,0] = fst' 1 ++ [0, sum (snd' 1), 0, 0, 0, 0, 0]
--   | ts!!1>0                          = fst' 1 ++ [(ts!!1)-100,(ts!!2)+100] ++ snd' 3
--   | ts!!0==200                       =           [0, 200, 0, 0, 0, 0, 0, 0]
--   | otherwise                        = [] where
--     fst' n  = fst $ splitAt n ts
--     snd' n  = snd $ splitAt n ts

-- downs :: [Int] -> [[Int]]
-- downs ts | next == [] = ts
--          | otherwise  = ts : downs next where
--   next = down ts
--
-- down :: [Int] -> [Int]
-- down ts | ts!!6 >  0 = [zipWith (+) ts [ 0, 0, 0, 0, 0, 0,-1, 2] | i<-[1..(ts!!6)]] -- 2p
--         | ts!!5 >  0 = [zipWith (+) ts [ 0, 0, 0, 0, 0,-1, 2, 1] | i<-[1..(ts!!6)]] -- 5p
--         | ts!!4 >  0 = [zipWith (+) ts [ 0, 0, 0, 0,-1, 2, 0, 0] | i<-[1..(ts!!6)]] -- 10p
--         | ts!!3 >  0 = [zipWith (+) ts [ 0, 0, 0,-1, 2, 0, 0, 0] | i<-[1..(ts!!6)]] -- 20p
--         | ts!!2 >  0 = [zipWith (+) ts [ 0, 0,-1, 2, 1, 0, 0, 0] | i<-[1..(ts!!6)]] -- 50p
--         | ts!!1 >  0 = [zipWith (+) ts [ 0,-1, 2, 0, 0, 0, 0, 0] | i<-[1..(ts!!6)]] -- 100p
--         | ts!!0 >  0 = [zipWith (+) ts [-1, 2, 0, 0, 0, 0, 0, 0] | i<-[1..(ts!!6)]] -- 200p
--         | otherwise  = []

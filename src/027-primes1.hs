-- ■ Problem 27 「二次式素数」
-- エラトステネスのふるいで[2..200000]までの素数を出すのにかかる時間を測定する。

import           Data.Set          (Set, empty, fromAscList, member, union)
import           System.Posix.Time (epochTime)

main :: IO ()
main = (timestamp . print .length . primes) 200000

-- エラトステネスのふるいによる素数列の取得
primes :: Int -> [Int] -- 素数の上限値
primes x = primes' x empty 2 where
  primes' :: Int -> Set Int -> Int -> [Int]
  primes' limit ngs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > limit      = []
    | n `member` ngs = primes' limit ngs (n+1)
    | otherwise      = n : primes' limit nextNgs (n+1)
    where nextNgs = ngs `union` fromAscList [n*2, n*3..limit]

-- あるIOアクションを実行するのにかかった時間を出力する。
timestamp :: IO a -> IO ()
timestamp act = do
  startTime<-epochTime
  act
  endTime<-epochTime
  putStrLn $ "time: " ++ show(endTime-startTime) ++ " sec."

import           Data.Set (Set, empty, filter, fromDistinctAscList, insert, map,
                           member, union)
import           Prelude  hiding (filter, map)

main = print $ answer 100000000

answer = sum . primeGens

-- m以下の素数生成整数を全て取得
primeGens :: Int -> Set Int
primeGens m = (filter areAllPrimes . filter (isPrimeD 2) . map (\x->x-1) . primesBySv) m  where
  isPrimeD d = isPrimeBySv m . numD d
  numD d = (+d) . (`div` d)
  areAllPrimes x = all (`isPrimeD` x) (divs x)
  divs x = filter (\y -> x `mod` y == 0) (fromDistinctAscList [1..rt x])
  rt = truncate . sqrt . fromIntegral

-- エラトステネスのふるい ----------------------------

isPrimeBySv x = (`member` primesBySv x)

primesBySv :: Int -> Set Int
primesBySv = pris' empty 2 where
  pris' :: Set Int -> Int -> Int -> Set Int
  pris' xs n s -- 取得した合成数のリスト、現在の数値
    | n > s         = empty
    | n `member` xs = pris' xs (n+1) s
    | otherwise     = insert n (pris' xsNx (n+1) s)
    where xsNx = xs `union` fromDistinctAscList [n*2, n*3..s]

{-
Problem 47 「異なる素因数」

1からNまでの素因数分解した結果を出力する。

使い方の例(N=10):

```sh
$ stack build ; stack exec 047-primeFactors 10

2
3
2 2
5
2 3
7
2 2 2
3 3
2 5
```

使い方の例(N=1000000):

```sh
$ stack build ; stack exec 047-primeFactors 1000000 > data/047-primeFactors.txt
```

100万まで15秒かかった。
-}

import           Data.Function      (on)
import           Data.List          (nubBy, sort)
import           Data.Set           (Set, empty, fromAscList, member, union)
import           System.Environment (getArgs)

type Fact=(Int,Int)

main = do
  rs<-getArgs
  let n = (read . head) rs
  (putStrLn . unlines . map (unwords . map show . toAry) . tail . facs) n

toAry :: [Fact] -> [Int]
toAry = foldr (\ x -> (++) (replicate (snd x) (fst x))) []

-- 素因数のリスト
facs :: Int -> [[Fact]]
facs n = (conv . trim . sort . facs') (pris n) where
  facs' [] = []
  facs' (p:ps) =  facsOfP 1 ++ facs' ps where
    -- 素数pのk乗を素因数にもつ合成数を追加
    facsOfP :: Int -> [(Int,Fact)]
    facsOfP k | p^k > n   = []
             | otherwise = [(i,(p,k)) | i<-[p^k,p^k*2..n]] ++ facsOfP (k+1)
  -- ある合成数の素数pについて、最大の冪乗だけ残す
  trim :: [(Int,Fact)] -> [(Int,Fact)]
  trim [] = []
  trim [x] = [x]
  trim (x:y:zs) | samePri x y = trim (y:zs)
                | otherwise = x:trim (y:zs)
    where samePri p q = ((==) `on` fst) p q && ((==) `on` (fst . snd)) p q

  {-
  (位置,値)のリストを、[値]のリストに変換する
  >>> conv [(1,3),(2,1),(2,2),(4,1)]
  [[],[3],[1,2],[],[1]]
  -}
  conv :: Ord a => [(Int,a)] -> [[a]]
  conv = conv' [] 0 . sort where
    conv' :: [a] -> Int -> [(Int,a)] -> [[a]]
    conv' prev _ [] = [prev]
    conv' prev p ((k,v):rs) | p==k = conv' (prev++[v]) p rs
                            | p/=k = prev: conv' [] (p+1) ((k,v):rs)

-- エラトステネスのふるいによる素数列の取得
pris :: Int -> [Int]
pris x = pris' x empty 2 where
  pris' :: Int -> Set Int -> Int -> [Int]
  pris' s xs n -- 素数の上限値、取得した合成数のリスト、現在の数値
    | n > s = []
    | n `member` xs = pris' s xs (n+1)
    | otherwise     = n : pris' s xsNx (n+1)
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

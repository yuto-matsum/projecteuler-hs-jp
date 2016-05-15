{-
Problem 47 「異なる素因数」
============================

問題
-----------

それぞれ2つの異なる素因数を持つ連続する2つの数が最初に現れるのは:

14 = 2 × 7
15 = 3 × 5

それぞれ3つの異なる素因数を持つ連続する3つの数が最初に現れるのは:

644 = 2^2 × 7 × 23
645 = 3 × 5 × 43
646 = 2 × 17 × 19

最初に現れるそれぞれ4つの異なる素因数を持つ連続する4つの数を求めよ. その最初の数はいくつか?

解答
----------------------

問題文から、答えは646より大きいだろう。

探索する上限Nを決めて、先に素数リストを作ってから探索する。

探索する上限Nの半分 N/2 までの素数をリストアップする: pris

N以下のの4つの整数 [x..x+3]について、普通に素因数分解する: spfc x

x = p_1^k_1 * p_2^k_2 ... *p_i^k_i
と分解できたとして、それを [(p_t,k_t)|t<-[1..i]] と表す。
例えば spfc 644 => [(2,2),(7,1),(23,1)]

この時、以下が成り立てばそのxが答え。

1. [x..x+3]のそれぞれの素因数の個数が全て4個
2. 任意の[x..x+3]の任意の素因数(p_t,k_t)について、他の素因数と互いに異なる。

--> spfcの素因数分解だと、N=10万までを完了するのに7時間くらいかかる。
しかも探索の結果、答えはそれ以上。
素因数分解の方法を変える必要がある。

1. N以下の素数リストを取得:pris
2. 素数pを因数にもつ数のリスト[p*2,p*3..N]を考える。
   このうち、p*pはp^2を素因数に持ち、p^k<=nまで同様に判定する必要がある。
3. 素因数リストfcs::[[Int]]に対して、2.で列挙したような素因数リストをunionしていく。

この素因数分解に時間がかかるし、他の問題にも利用出来ると考えられるので
素因数分解した結果をファイル出力するようにしてみる。
(047-primeFactors.hs)

```sh
$ stack exec 047-primeFactors 1000000 > data/047-primeFactors.txt
$ stack exec 047 1000 3 1 < data/047-primeFactors.txt
Just 644
```

--> N=100万、かつ10万からスタートして、4分半で完了した。

```sh
$ stack exec 047 1000000 4 100000 < data/047-primeFactors.txt
```
-}

import           Data.Function      (on)
import           Data.List          (nubBy, sort)
import           Data.Set           (Set, empty, fromAscList, fromList, member,
                                     size, union)
import           System.Environment (getArgs)

-- main = print $ search 10000 4 644

main = do
  cs<-getContents
  rs<-getArgs
  let facs=toFacts cs
      n=(read . head) rs
      l=(read . (!!1)) rs
      s=(read . (!!2)) rs
  print $ search facs n l s

type Fact=(Int,Int)

toFacts :: String -> [[Fact]]
toFacts = map (toFact . map read . words) . ("":) . lines

toFact :: [Int] -> [Fact]
toFact [] = []
toFact (x:xs) = toFact' (x,1) xs
  where
    toFact' (p,k) [] = [(p,k)]
    toFact' (p,k) (x:xs) | p==x = toFact' (p,k+1) xs
                         | p/=x = (p,k): toFact' (x,1) xs

search :: [[Fact]] -> Int -> Int -> Int -> Maybe Int
search facs n l x
  | n < lst   = Nothing
  | right     = Just x
  | otherwise = search facs n l (x+1)
  where
    lst = x+l-1
    right = eval [facs !! i | i<-[x..lst]]
    eval y = evalLen l y && evalFct y

-- 素因数tsの個数が全てl個であるか判定
evalLen :: Int -> [[Fact]] -> Bool
evalLen l = all ((==l) . length)

-- 素因数が互いに異なるか判定
evalFct :: [[Fact]] -> Bool
-- evalFct x = (length . concat) x == (size . fromList . concat) x
-- evalFct x = (sum . map length) x == (size . foldl union empty . map fromAscList) x
evalFct = not . dup . sort . concat

-- 同じ要素を持っているか判定
dup :: Eq a => [a] -> Bool
dup xs = or (zipWith (==) xs (tail xs))

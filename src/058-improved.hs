{-

素数判定方について、エラトステネスのふるいではなく確率的判定方法を使って高速化を図る。

ミラーラビンテスト:

1. 判定したい数が偶数の場合、2以外は合成数。

2. 判定したい正の奇数をxとした場合、以下を満たす自然数s,奇数dを求める。
    x-1 = 2^s * d

3. 以下を満たす整数aを適当に1つ定める。
    0 < a < x,
    a^d `mod` x /= 1

4. 以下を満たすならxは素数でない。満たさないなら素数かも。
    0 <= r <= s-1 となるすべての整数rで以下が成り立つ。
    a^(2^r * d) `mod` x /= x-1
-}

import           Data.List  (find)
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Set   (Set, empty, fromAscList, insert, member, size,
                             toAscList, union)

main = mapM_ (print . answer) [1/2, 1/3, 1/4, 1/5, 1/6, 1/7, 1/8, 1/9, 1/10]

toF = fromIntegral

-- 螺旋素数を小さい層から順に調べて、素数の割合がr未満になる辺の長さと素数の数を返却
answer :: Double -> Maybe (Int, Int)
answer r = (find ((r>) . rate) . tail) spirals

-- (辺の長さ,対角線上の素数の個数)から、素数の割合を返却
rate :: (Int,Int) -> Double
rate (x,y) = toF y / (toF . crossLength) x

-- 螺旋素数
spirals :: [(Int,Int)]
spirals = iterate nextRound (1,0)

-- (辺の長さ, 対角線上の素数の個数)を受け取り、次の層を追加した時のそれを返却
-- nextRound (5,5) = (7,8)
nextRound :: (Int,Int) -> (Int,Int)
nextRound (x,y) = (x+2, y+delta) where
  delta = (length . filter isPrime) cans
  cans = (take 3 . tail . iterate (x+1+)) (x^2)

-- 辺の長さxにおける対角線の長さ
crossLength :: Int -> Int
crossLength x = 2*x-1

-- xが素数か判定
isPrime :: Int -> Bool
isPrime = isPrimeByMR 3

{- べき剰余　pythonのpowメソッドのクローン
>>> pow 1234 5678 9 == fromIntegral (1234 ^ 5678 `mod` 9)
True
-}
pow :: Int -> Int -> Int -> Int
pow _ 0 _ = 1
pow b x m | odd  x = b * pow b (x-1) m `mod` m
          | even x = pow b (x`div`2) m ^2 `mod` m

-- フェルマーテスト ------------------------------------

-- N回のフェルマーテストによって、数xが素数か判定する。
isPrimeByFm :: Int -> Int -> Bool
isPrimeByFm n x
  | x < 2 = False
  | otherwise = never isMultiple wits where
  wits = (take n . fmWits) x
  isMultiple = fermat x

-- フェルマーテストによって、数xが合成数か証人aを用いて判定
fermat :: Int -> Int -> Bool
fermat x a = pow a (x-1) x /= 1

-- フェルマーテストにおける、数xの証人に使える数のリスト
-- xと互いに素である数
fmWits :: Int -> [Int]
fmWits x = filter ((==1) . gcd x) [2..(x-1)]

-- ミラー・ラビンテスト ------------------------------------

-- N回のミラー・ラビンテストによって、数xが素数か判定
isPrimeByMR :: Int -> Int -> Bool
isPrimeByMR n x
  | x < 2  = False
  | even x = x==2
  -- N個の証人に対して、一度も合成数と判定しなかったら素数と判断
  | odd x  = never isMultiple wits where
  wits = (take n . mrWits) x
  isMultiple = milrab x

-- 一つでもTrueがあればFalseを返却
never :: (a->Bool) -> [a] -> Bool
never f = not . any f

-- ミラー・ラビンテストを用いて、奇数xが合成数か証人aを用いて判定
milrab :: Int -> Int -> Bool
milrab x a = (x-1) `notElem` pow2s where
  (s,d) = splitForMR x
  pow2s = (take s . iterate pow2 . pow a d) x
  pow2 i = pow i 2 x

-- 素数判定したい奇数xに対して x-1 == 2^s*d を満たす自然数sと奇数dを取得
splitForMR :: Int -> (Int,Int)
splitForMR x = divBy2Pow 1 ((x-1) `div` 2) where
  divBy2Pow s d | odd d     = (s,d)
                | otherwise = divBy2Pow (s+1) (d `div` 2)

-- ミラー・ラビンテストにおける、数xの証人に使える数のリスト
-- w^d `mod` x /= 1を満たす数w
mrWits :: Int -> [Int]
mrWits x = [w | w<-[2..(x-1)], pow w d x /= 1] where
  d = (snd . splitForMR) x

-- 底が2の対数の整数部分
log2 :: Int -> Int
log2 = truncate . logBase 2.0 . toF

-- エラトステネスのふるい ----------------------------

isPrimeBySv x = (`elem` primesBySv x)

primesBySv :: Int -> [Int]
primesBySv x = toAscList (pris' empty 2 x) where
  pris' :: Set Int -> Int -> Int -> Set Int
  pris' xs n s -- 取得した合成数のリスト、現在の数値
    | n > s         = empty
    | n `member` xs = pris' xs (n+1) s
    | otherwise     = insert n (pris' xsNx (n+1) s)
    where xsNx = xs `union` fromAscList [n*2, n*3..s]

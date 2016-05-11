{-
■ Problem 44 「五角数」
=======================

■ 問題
-----------

五角数は Pn = n(3n-1)/2 で生成される. 最初の10項は
1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
である.

P4 + P7 = 22 + 70 = 92 = P8 である.
しかし差 70 - 22 = 48 は五角数ではない.

五角数のペア Pj と Pk について, 差と和が五角数になるものを考える.
差を D = |Pk - Pj| と書く. 差 D の最小値を求めよ.

■ 解答
--------------

紙とペンで式をいじくる作業がほとんど。

■ 五角数の差と和の性質
---------------------

まず、五角数は単調増加するため、 k<j の時、 |Pk - Pj| = Pj - Pk > 0 となる。

Piと、1つ前 P(i-1)との差をD1とすると、
D1 = Pi - P(i-1)
= i(3i-1)/2 - (i-1)(3(i-1)-1)/2
= 3i - 2

j個前との差 Dj は
Dj = Pi - P(i-j)
= 3ji -j(3j+1)/2

と書ける。(ただし j<i)

一方、Piとj個前との和を Sj とすると
Sj = Pi + P(i-j)
= 3ii - (3j+1)i + j(3j+1)/2
= 2Pi - Di
(これも j<i)

なお、DjとD(j-1)を比べると、iが等しい場合に Dj > D(j-1) が成り立つ。
また、当然だが iが小さい方がDも小さい。
そのため、i,jが小さい範囲で五角数を判定する必要がある。

■ 五角数の判定
-------------

次に、ある自然数xが五角数であることを判定する条件を考える。

x = n(3n-1)/2, nは自然数
=> 3nn - n -2x = 0
=> n = (1+√(24x+1))/6

つまり、以下2つの条件を満たせば、xは五角数。
- 条件1: 24x+1 が平方数 [1,4,9,25,..]
- 条件2: 1+√(24x+1)が6の倍数

試しに検算してみる。
1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
x=1: 1+5=6: OK
x=92: 1+√(24*92+1)=48: OK

■ 計算方針
----------------

式の性質をもう少し見れば、計算機を使わなくても解答できるんじゃないかと思ったがうまくいかなかった。
とりあえず 2<=i<=100, 1<=j<iまでの Dj, Sjを計算し、五角数の判定をしてみることにする。
--> ヒットするD,Sが出てこない。i<1000でもダメ。
--> i<3000で出てきた。計算時間は1秒くらい。
-}

import           Data.Function (on)
import           Data.List     (minimumBy)

main = print answer

answer :: ((Int,Int), (D,S))
answer = (minD . pents) 3000

minD :: [((Int,Int), (D,S))] -> ((Int,Int), (D,S))
minD = minimumBy (compare `on` (fst . snd))

pents :: Int -> [((Int,Int), (D,S))]
pents x =  filter (isPentDS . snd) (points x)

type D =Int
type S =Int

points :: Int -> [((Int,Int), (D,S))]
points x = [((i,j), (difp i j, sump i j)) | i<-[2..x], j<-[1..(i-1)]]

pent :: Int -> Int
pent n = n * (3*n - 1) `div` 2

difp :: Int -> Int -> D
difp i j = 3*j*i -j*(3*j+1) `div` 2

sump :: Int -> Int -> S
sump i j = pent i * 2 - difp i j

isPentDS :: (D,S) -> Bool
isPentDS x = (isPent . fst) x && (isPent . snd) x

isPent :: Int -> Bool
isPent x = (isSq . inrt) x && (is6mul . (+1) . sqrt' . inrt) x where
    inrt = (+1) . (*24)
    sqrt' = truncate . sqrt . fromIntegral
    isSq x = ((==x) . (^2) . sqrt') x
    is6mul = (==0) . (`mod` 6)

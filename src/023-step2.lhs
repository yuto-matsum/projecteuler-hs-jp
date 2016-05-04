■ Problem 23 「非過剰数和」
======================

■ 解答
----------------------

使用するモジュール

> import Data.List (sort, nub)

以下のステップ2を計算する。

1. 過剰数をリストアップ
2. 過剰数リストの任意の2要素の和をリストアップ
3. [1..28123]から過剰数和のリストの差集合を出し、和を算出

■ 2. 過剰数リストの任意の2要素の和をリストアップ

入力で過剰数リストを受け取って、2つの過剰数の和のリストを出力する。

> main :: IO ()
> main = do

>   abusTxt <- getContents
>   let theAbus = (map read . lines) abusTxt :: [Int]
>       s = length theAbus - 1
>       sumAbus = [theAbus !! i + theAbus !! j | i <-[0..s], j <- [i..s]]
>   mapM_ print ((nub . sort . filter (<=28123)) sumAbus)

O(N^2 logN)。15分くらいかかった。
`data/023-abus.txt`を入力して`data/023-sumabus.txt`を出力するなら、以下のとおり。

```bash
stack build :023-step2 ; stack exec 023-step2 <data/023-abus.txt >data/023-sumabus.txt
```

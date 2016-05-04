■ Problem 23 「非過剰数和」
======================

■ 解答
----------------------

使用するモジュール

> import Data.List ((\\))

以下のステップ3を計算する。

1. 過剰数をリストアップ
2. 過剰数リストの任意の2要素の和をリストアップ
3. [1..28123]から過剰数和のリストの差集合を出し、和を算出

■ 3. [1..28123]から過剰数和のリストの差集合を出し、和を算出

入力で過剰数の和のリストを受け取って、過剰数の和で書けない数の総和を出力する。

> main :: IO ()
> main = do
>   sumAbusTxt <- getContents
>   let sumAbus = (map read . lines) sumAbusTxt :: [Int]
>       nonSumAbus = [1..28123] \\ sumAbus
>   print (sum nonSumAbus)

`data/023-sumabus.txt`を入力するなら以下のとおり。

```bash
stack build :023-step3 ; stack exec 023-step3 < data/023-sumabus.txt
```

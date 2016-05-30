{-
Problem 63 「べき乗の桁の個数」
==============================
5桁の数 16807 = 75は自然数を5乗した数である.
同様に9桁の数 134217728 = 89も自然数を9乗した数である.

自然数を n 乗して得られる n 桁の正整数は何個あるか?

解答
-----------

べき乗を扱うのでIntegerで実装。

まず、N乗してN桁になる数のリストを作ってみる。

1乗して1桁:[1..9]
2乗して2桁:[4..9]
...

問題文から、Nがある程度大きいとN桁になる数がなくなると考えられる。

-}

main = do
  let n=50 -- ひとまず50乗まで
  print $ takeWhile (not . null) (map powdigits [1..n])

-- N乗してN桁になる数のリストを返却
powdigits :: (Integral a, Show a) => a -> [a]
powdigits x = pickBy sameDigit [1..9] -- 10以上は常に合わない
  where
    sameDigit :: (Integral a, Show a) => a -> Bool
    sameDigit = (x==) . fromIntegral . length . show . (^x)

pickBy f = takeWhile f . dropWhile (not . f)

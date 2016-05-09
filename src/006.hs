
main=undefined
answer :: Integral a => a -> a
answer n = (sum [1..n])^2 - sum(map double [1..n])
  where double i = i^2

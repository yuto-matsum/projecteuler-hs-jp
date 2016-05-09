
main=undefined
primes = 2 : f [3] [3,5..]
  where
    f (x:xs) ys = let (smls, bigs) = span (< x^2) ys
                  in  smls ++ f (xs ++ smls) [z | z <- bigs, mod z x /= 0]

answer n = sum $ takeWhile (<n) $ primes

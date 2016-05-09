main=undefined
primes1 = 2 : f [3,5..]
  where f(x:xs) = x : f [y | y<-xs, mod y x /= 0]

primes2 = 2 : filter f [3,5..]
  where f n = allMod0 n $ f' n
        allMod0 n = all $ (/= 0) . (mod n)
        f' n = takeWhile ((<= n) . (^ 2)) primes2

primes3 = 2 : f [3] [3,5..]
  where
    f (x:xs) ys = let (smls, bigs) = span (< x^2) ys
                  in  smls ++ f (xs ++ smls) [z | z <- bigs, mod z x /= 0]

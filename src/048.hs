
main=undefined
answer = sum [powMod (i,lastTen) i | i<-[1..1000]] `mod` lastTen where
  lastTen = 10^10

-- mod after power
powMod :: Integral a => (a,a) -> a -> a
powMod (x,y) i = let prev = if i>1 then powMod (x,y) (i-1) else 1
                 in mod (prev * x) y

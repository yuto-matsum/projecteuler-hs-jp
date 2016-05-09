--import MyLib
main=print $ cmb (20*2) 20
{-|
>>> cmb 5 4 == (5*4*3*2) `div` (1*2*3*4)
True
-}
cmb :: Integral a => a -> a -> a
cmb i j = div (product [(i-j+1)..i]) (product [1..j])

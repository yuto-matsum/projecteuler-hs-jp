import MyLib (tris,pnts,hexs,elemBin)
import System.Environment

answer n = (filterTri.filterPnt) hexs' where
  tris' = take (n*4)       tris
  pnts' = take (n*4`div`3) pnts
  hexs' = take n           hexs
  filterTri = filter (\x-> elemBin x tris')
  filterPnt = filter (\x-> elemBin x pnts')

-- main = do
--        args <- getArgs
--        let n = read $ head args
--        print $ answer n

-- tri: (1/2)ii+(1/2)i-a=0: i= sqrt(2a+0.25)-0.5
-- pnt: (3/2)ii-(1/2)i-a=0: i=(sqrt(6a+0.25)+0.5)/3
-- hex: 2ii-i-a=0:          i=(sqrt(8a+1)   +1)/4

answer' = triPntHexs!!2 where
  triPntHexs = filter (\x->isTri x&&isPnt x) hexs

isTri n = isSeisu $  sqrt(2*fi n+0.25)-0.5
isPnt n = isSeisu $ (sqrt(6*fi n+0.25)+0.5)/3
isHex n = isSeisu $ (sqrt(8*fi n+1   )+1  )/4

isSeisu :: RealFrac a => a -> Bool
isSeisu x = x - fromIntegral(truncate x) == 0

fi = fromIntegral

main = print answer'


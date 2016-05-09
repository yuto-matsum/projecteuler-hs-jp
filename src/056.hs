import           MyLib (qsortBy)

main=undefined
fi::(Integral a, Num b)=>a->b
fi=fromIntegral

pow :: (Integral a, Integral b)=>a->Int->b
pow x y = (product . map fi . take y . repeat) x

sumdig :: (Integral a, Integral b)=>a->b
sumdig x | x<10=fi x
         | otherwise=fi(x`mod`10)+sumdig(x`div`10)

answer :: (Integral a)=>Int->[(Int,Int,a)]
answer n = sort' [(i,j,s i j)|i<-[1..n],j<-[1..n]] where
  sort' = qsortBy (\(_,_,x)(_,_,y)->compare x y)
  s i j = sumdig(pow i j)

--  writeFile "out.txt" ((unlines.map show.answer)99)

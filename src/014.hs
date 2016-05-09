import           System.Environment

clz :: Integral a => a -> [a]
clz i | i == 1    = [1]
      | even i    = i:clz (div i 2)
      | otherwise = i:clz (3*i+1)

lenclz i=length $ clz i

answer i= maximum [(lenclz i,i)|i<-[1000000,999999..i]]

main = do
  args <- getArgs
  print $ answer $ read (head args)

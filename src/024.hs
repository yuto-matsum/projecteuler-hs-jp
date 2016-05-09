import           Data.List          (delete, foldl')
import           System.Environment

cmb :: (Eq a) => [a] -> [ [a] ]
cmb [] = [[]]
cmb xs = [ys | x<-xs, ys<-map (x:) (cmb (delete x xs))]

answer n = cmb [i|i<-['0'..'9']] !! (n-1)

main = do
  args <- getArgs
  let n = read (head args)
  print $ answer n

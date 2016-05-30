{-
Problem 81 「経路の和:2方向」
==============================

下記の5次の正方行列で, 左上のセルから開始し右下のセルで終わるパスを探索する. ただし下方向と右方向にのみ移動できるものとする.
通過したセルの和が最小となるパスは赤の太字で示されたもので, その値は2427である.

131	673	234	103	18
201	96	342	965	150
630	803	746	422	111
537	699	497	121	956
805	732	524	37	331
今, 31Kのテキストファイルmatrix.txt (右クリックして, 『名前をつけてリンク先を保存』)には80×80の行列が書かれている.
同様に左上のセルから開始し右下のセルで終わり, かつ右方向と下方向にのみ移動するときの最小のパスの和を求めよ.

解答
-------------

経路探索問題。
循環路がないので、スタートから全方向へ部分経路の最短経路長を求めていけば、
ゴールまでの最短系路長がでてくる。

実装するべきは以下

* 入力ファイルから読み込んで、有向グラフにデータ型を変換
* 部分経路の最短経路長の計算

--> n=15以降がとても時間が掛かる。
遅延評価に頼りすぎて、二重計算をしているみたい。

X方向に一行ずつ判定し、古い行を廃棄していく形にすれば、二重計算が省けるかも。

-}
import           Data.Function (on)
import           Data.List     (minimumBy, union)
-- import           Data.List.Split (chunksOf)

type Matrix a = [[a]]
type Position = (Int,Int)

-- 重みの文字列を行列の数値に変換
weights :: String -> Matrix Int
weights = map (map read . wordsByComma) . lines

-- カンマでリスト化
wordsByComma :: String -> [String]
wordsByComma x
  | w == x = [x]
  | otherwise = w : wordsByComma (drop (length w + 1) x) where
  w = takeWhile (/=',') x

-- Matrix内の、Positionにある値を返却
get :: Position -> Matrix a -> a
get (x,y) = (!!x) . (!!y)

-- 案1 とりあえず書いてみた ---------------------------------------------
-- --> N=15くらいですごい遅くなる
-- main = do
--   txt <- readFile "data/p081_matrix.txt"
--   let ws = weights txt
--       width = (length . head) ws
--       height = length ws
--       ss = srcs width height
--   mapM_ print minPath ws ss [(i,i)|i<-[0..width-1]]

-- srcs :: Int -> Int -> Matrix [Position]
-- srcs w h = unionM (right (slfs w h)) (down (slfs w h))

-- dsts :: Int -> Int -> Matrix [Position]
-- dsts w h = unionM (left (slfs w h)) (up (slfs w h))

-- slfs :: Int -> Int -> Matrix [Position]
-- slfs w h = chunksOf w [[(x,y)]|y<-[0..h-1],x<-[0..w-1]]

-- chunksOf :: Int -> [a] -> [[a]]
-- chunksOf _ [] = []
-- chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- right :: Matrix [a] -> Matrix [a]
-- right = map (([]:) . takeWithoutLast)

-- down :: Matrix [a] -> Matrix [a]
-- down x = emptyLine x : takeWithoutLast x

-- left :: Matrix [a] -> Matrix [a]
-- left = map ((++[[]]) . tail)

-- up :: Matrix [a] -> Matrix [a]
-- up x = tail x ++ [emptyLine x]

-- unionM :: Matrix [a] -> Matrix [a] -> Matrix [a]
-- unionM [] [] = []
-- unionM x y = zipWith (++) (head x) (head y) : unionM (tail x) (tail y)

-- emptyLine :: Matrix [a] -> Matrix a
-- emptyLine x = replicate ((length . head) x) []

-- takeWithoutLast :: [a] -> [a]
-- takeWithoutLast x = take (length x - 1) x

-- type Path = (Int,[Position])
-- minPath :: Matrix Int -> Matrix [Position] -> Position -> Path
-- minPath ws ss p
--   | null s    = (w,[p])
--   | otherwise = (proceed . minLen) (map (minPath ws ss) s) where
--   proceed (pw,ps) = (pw+w, ps++[p])
--   minLen = minimumBy (compare `on` fst)
--   w = get p ws
--   s = get p ss

-- 案2 srcを使わない簡易版----------------------------------------------
-- --> 案１より特殊化したため簡潔だが、速度は変わらず

-- main = do
--   txt <- readFile "data/p081_matrix.txt"
--   let ws = weights txt
--       ls = map (mp ws) [(79,i) | i<-[0..79]]
--   mapM_ print ls

-- mp :: Matrix Int -> Position -> Int
-- mp ws (x,y)
--   | x>0 && y>0 = get (x,y) ws + min (mp ws (x-1,y)) (mp ws (x,y-1))
--   | x>0        = get (x,y) ws + mp ws (x-1,y)
--   | y>0        = get (x,y) ws + mp ws (x,y-1)
--   | otherwise  = get (x,y) ws

-- 案3 メモリに保持するように一行ずつ評価する ----------------------
-- --> 2秒で出た。

main = do
  txt <- readFile "data/p081_matrix.txt"
  let ws = weights txt
      n = (length . head) ws
  print $ foldl minLen (0: replicate (n-1) 999999) ws

minLen :: [Int] -> [Int] -> [Int]
minLen ls ws = map mins [0..length ls - 1] where
  mins 0 = head ls + head ws
  mins x = min (ls!!x + ws!!x) (mins (x-1) + ws!!x)

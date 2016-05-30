{-
Problem 54 「ポーカーハンド」
===================================
カードゲームのポーカーでは, 手札は5枚のカードからなりランク付けされている.
役を低い方から高い方へ順に並べると以下である.

* 役無し(ハイカード): 一番値が大きいカード
* ワン・ペア: 同じ値のカードが2枚
* ツー・ペア: 2つの異なる値のペア
* スリーカード: 同じ値のカードが3枚
* ストレート: 5枚の連続する値のカード
* フラッシュ: 全てのカードが同じスート
  (注: スートとはダイヤ・ハート・クラブ/スペードというカードの絵柄のこと)
* フルハウス: スリーカードとペア
* フォーカード: 同じ値のカードが4枚
* ストレートフラッシュ: ストレートかつフラッシュ
* ロイヤルフラッシュ: 同じスートの10, J, Q, K, A

ここでカードの値は小さい方から2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q, K, Aである.
(訳注：データ中で10は'T'と表される)

もし2人のプレイヤーが同じ役の場合には, 役を構成する中で値が最も大きいカードによってランクが決まる:
例えば, 8のペアは5のペアより強い (下の例1を見よ).
それでも同じランクの場合には (例えば, 両者ともQのペアの場合), 一番値が大きいカードによってランクが決まる (下の例4を見よ).
一番値が大きいカードが同じ場合には, 次に値が大きいカードが比べれられ, 以下同様にランクを決定する.

例:

|試合 |プレイヤー1      |プレイヤー2       |勝者        |
|----|----------------|----------------|-----------|
|1   | 5H 5C 6S 7S KD | 2C 3S 8S 8D TD | プレイヤー2 |
|    | 5のペア         | 8のペア         |           |
|2   | 5D 8C 9S JS AC | 2C 5C 7D 8S QH | プレイヤー1 |
|    | 役無し, A       | 役無し, Q       |           |
|3   | 2D 9C AS AH AC | 3D 6D 7D TD QD | プレイヤー2 |
|    | Aのスリーカード  | ダイヤのフラッシュ |           |
|4   | 4D 6S 9H QH QC | 3D 6D 7H QD QS |プレイヤー1  |
|    | Qのペア, 9      | Qのペア, 7      |           |
|5   | 2H 2D 4C 4D 4S | 3C 3D 3S 9S 9D |プレイヤー1  |
|    |4-2のフルハウス   | 3-9のフルハウス  |           |

poker.txtには1000個のランダムな手札の組が含まれている.
各行は10枚のカードからなる (スペースで区切られている):
最初の5枚がプレイヤー1の手札であり, 残りの5枚がプレイヤー2の手札である.
以下のことを仮定してよい

全ての手札は正しい (使われない文字が出現しない. 同じカードは繰り返されない)
各プレイヤーの手札は特に決まった順に並んでいるわけではない
各勝負で勝敗は必ず決まる

1000回中プレイヤー1が勝つのは何回か?
(訳注 : この問題に置いてA 2 3 4 5というストレートは考えなくてもよい)
-}

import           Control.Arrow ((***))
import           Data.Function (on)
import           Data.List     (elemIndex, nub, sort, sortBy, sortOn)
import           Data.Maybe    (fromJust)

main=do
  txt<-readFile "data/p054_poker.txt"
  let rounds = (map getHands . lines) txt
      answer = (length . filter winP1) rounds
  print answer

type Card = (Int,Suit)
type Hand = [Card]

-- 一行の文字列を受け取って手札型に変換
getHands :: String -> (Hand,Hand)
getHands x = ((sort . map toCard . take 5 . words) x, (sort . map toCard . drop 5 . words) x)

-- P1が勝っているか判定
winP1 :: (Hand,Hand) -> Bool
winP1 x = winByRank || winByNum where
  winByRank    = p1rank >  p2rank
  winByNum     = p1rank == p2rank && p1nums > p2nums
  (p1rank, p2rank)       = (getRank *** getRank) x
  (p1nums, p2nums)       = (getNums p1rank *** getNums p2rank) x

-- 2文字をカード型に変換
toCard :: String -> Card
toCard x = ((toNum . head) x , (toSuit . last) x)

-- 1文字を数字に変換、Aは14とする
toNum :: Char -> Int
toNum x = ((+2) . fromJust . elemIndex x) "23456789TJQKA"

-- 絵柄、Readできるようにする
data Suit = D | H | C | S deriving (Eq, Ord, Read, Show)

-- 1文字を絵柄に変換
toSuit :: Char -> Suit
toSuit = read . (:[])

-- 役の種類、大きい方が強い
data Rank = HighCard | OnePair | TwoPairs | ThreeKind | Straight | Flush
          | FullHouse | FourKind | StraightFlush | RoyalFlush
  deriving (Eq, Ord, Show)

-- 手札から役を取得
getRank :: Hand -> Rank
getRank x = search x rankConds where
  search x (c:cs) = if fst c x then snd c else search x cs

-- 数字の強さをリストで表現：役の数字(フルハウスなら3カードが先)、役以外の数字の降順
getNums :: Rank->Hand->[Int]
getNums r x
  | rankOfPairs r = concatMap (`kindNumOf` x) [4,3,2,1]
  | otherwise     = (sortDesc . nums) x

-- ペアが含まれる役か判定
rankOfPairs :: Rank->Bool
rankOfPairs = (`elem` [OnePair, TwoPairs, ThreeKind, FullHouse, FourKind])

-- Nペアの数字を取得、2ペアの場合は複数あり得るのでリストで降順に取得
kindNumOf :: Int -> Hand -> [Int]
kindNumOf n = sortDesc . map fst . filter ((==n) . snd) . count . nums

-- 役と、役の説明のセット：条件の厳しい順に判定
rankConds :: [(Hand->Bool, Rank)]
rankConds = [
  (allCond [isStraight, oneSuit, (==10) . head . nums], RoyalFlush),
  (allCond [isStraight, oneSuit]                      , StraightFlush),
  ((==4) . maximum . numOfAKind                       , FourKind),
  ((==[3,2]) . take 2 . sortDesc . numOfAKind         , FullHouse),
  (oneSuit                                            , Flush),
  (isStraight                                         , Straight),
  ((==3) . maximum . numOfAKind                       , ThreeKind),
  ((==[2,2]) . take 2 . sortDesc . numOfAKind         , TwoPairs),
  ((==2) . maximum . numOfAKind                       , OnePair),
  (const True                                         , HighCard)]

-- 降順ソート
sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)

-- 値xが、指定したすべての条件を満たすか判定
allCond :: [a->Bool] -> a -> Bool
allCond fs x = all (\f->f x) fs

-- ペアの数を取得
numOfAKind :: Hand -> [Int]
numOfAKind = map snd . count . nums

{-
>>> count [1,5,1,4,4,2,4]
[(4,3),(1,2),(2,1),(5,1)]
-}
count :: Ord a => [a] -> [(a,Int)]
count = reverse . sortOn snd . count' . (`zip` repeat 1) . sort where
  count' :: Ord a => [(a,Int)] -> [(a,Int)]
  count' []  = []
  count' [x] = [x]
  count' ((x,i):(y,j):zs) | x==y      = count' ((x,i+j):zs)
                          | otherwise = (x,i): count' ((y,j):zs)

-- 5枚のカードが階段になっているか
isStraight :: Hand -> Bool
isStraight x = let ns=nums x in zipWith (-) (tail ns) ns == [1,1,1,1]

-- 5枚のカードの数字を取得
nums :: Hand -> [Int]
nums = map fst

-- 5枚のカードの絵柄を取得
suits :: Hand -> [Suit]
suits = map snd

-- 5枚のカードを絵柄が全て同じか判定
oneSuit :: Hand -> Bool
oneSuit = (==1) . length . nub . suits

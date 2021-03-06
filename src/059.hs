{-
Problem 59 「XOR暗号解読」
================================

問題
--------------

(訳者注: 文字コードの説明は適当です) 各文字はそれぞれ一意のコードに割り当てられている.
よく使われる標準としてASCII (American Standard Code for Information Interchange) がある.
ASCIIでは, 大文字A = 65, アスタリスク (*) = 42, 小文字k = 107というふうに割り当てられている.

モダンな暗号化の方法として, テキストファイルの各バイトをASCIIに変換し,
秘密鍵から計算された値とXORを取るという手法がある.
XOR関数の良い点は, 暗号化に用いたのと同じ暗号化鍵でXORを取ると平文を復号できる点である.
65 XOR 42 = 107であり, 107 XOR 42 = 65である.

破られない暗号化のためには, 鍵は平文と同じ長さのランダムなバイト列でなければならない.
ユーザーは暗号文と暗号化鍵を別々の場所に保存する必要がある.
また, もし一方が失われると, 暗号文を復号することは不可能になる.

悲しいかな, この手法はほとんどのユーザーにとって非現実的である.
そこで, 鍵の変わりにパスワードを用いる手法が用いられる.
パスワードが平文より短ければ (よくあることだが), パスワードは鍵として繰り返し用いられる.
この手法では, 安全性を保つために十分長いパスワードを用いる必要があるが,
記憶するためにはある程度短くないといけない.

この問題での課題は簡単になっている. 暗号化鍵は3文字の小文字である.
cipher1.txtは暗号化されたASCIIのコードを含んでいる.
また, 平文はよく用いられる英単語を含んでいる.
この暗号文を復号し, 平文のASCIIでの値の和を求めよ.

解答
-----------

鍵の性質について:

鍵は3文字の小文字。

map ord "az" == [97,122]
なので、鍵はそれぞれ97以上122以下.

文字   10進        2進
'a':    97: 0110 0001
'z':   122: 0111 1010


XORの逆関数:

k1を求めるために、xorの逆関数、つまり
c xor k = dに対する
d rxor c = kとなる rxorを考える。

結論を言ってしまうと、rxor == xor.


頻度分析:

cipher1.txtを3個のリストに分配し、数の頻度をそれぞれ解析してみる。
以下のような感じで。

1,2,3,4,5,6,1,2,5
3リストに分配 -> [1,4,1],[2,5,2],[3,6,5]
頻度解析 -> [(1,2),(4,1)],[(2,2),(5,1)],[(3,1),(6,1),(5,1)]

059-analysis.hs参照
結果は以下。

k1: [("71",70),("2",48),("9",28),("19",28),("15",25),("8",18),("6",18),("14",18),("20",17),("21",15),("11",12),("0",12),("73",10),("3",8),("16",7),("10",6),("1",6),("75",5),("5",5),("4",5),("18",5),("32",4),("17",4),("45",3),("30",3),("23",3),("86",2),("31",2),("12",2),("92",1),("87",1),("84",1),("83",1),("79",1),("78",1),("73\n",1),("64",1),("52",1),("41",1),("37",1),("33",1)]
k2: [("79",85),("7",35),("10",31),("6",28),("0",26),("27",21),("28",20),("14",20),("3",18),("11",16),("1",16),("24",9),("8",8),("2",8),("9",6),("29",6),("26",6),("25",6),("22",5),("12",5),("59",4),("13",3),("67",2),("65",2),("56",2),("39",2),("31",2),("94",1),("91",1),("89",1),("87",1),("72",1),("60",1),("45",1),("38",1)]
k3: [("68",77),("1",41),("16",31),("11",26),("12",24),("10",23),("13",21),("22",17),("5",16),("23",14),("3",11),("0",11),("8",8),("9",7),("19",7),("7",6),("85",5),("29",5),("20",5),("18",5),("17",5),("74",4),("72",4),("2",4),("6",3),("35",3),("86",2),("44",2),("93",1),("87",1),("83",1),("81",1),("69",1),("48",1),("40",1),("38",1),("37",1),("33",1),("30",1),("28",1),("15",1)]


頻度分析の考察:

英単語の最多の文字は"e"だと予想する。

"e"のASCIIコードは: ord 'e' == 101

=> k1 = 101 `xor` 71 = 34

=> 小文字じゃない(97以上122以下じゃない)ので予想が間違っている。

次の多そうなのは半角スペース。

ord ' ' == 32

=> k1 = 32 `xor` 71 = 103 => 'g' これかも

k2, k3も同様に見てみる。
79が半角スペースとすると、 k2 = 32 xor 79 = 111
68が半角スペースとすると、 k3 = 32 xor 68 = 100
-}
import           Data.Bits       (xor)
import           Data.Char       (chr, ord)
import           Data.List       (sort, sortOn)
import           Data.List.Split (splitEvery, splitOn)
main = do
  txt <- readFile "data/p059_cipher.txt"
  let code = (map read . splitOn ",") txt :: [Int]
      len = length code
      key = (take len . concat . repeat . map ord) "god"
      dec = zipWith xor code key
  print $ sum dec

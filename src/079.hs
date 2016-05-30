{-
Problem 79 「パスコードの導出」
===============================

オンラインバンクで通常使われるsecurity methodは, パスコードからランダムに選んだ3文字を
ユーザーに要求するものである.

たとえば, パスコードが531278のとき, 2番目, 3番目, 5番目の文字を要求されるかもしれない.
このとき, 期待される答えは: 317 である.

テキストファイルkeylog.txtには, ログインに成功した50回の試行が記録されている.

3つの文字が常に順番通りに要求されるとするとき, ファイルを分析して,
可能なパスコードのなかでもっとも短いものを見つけよ.

解答
===============

試行317が成功した時、パスコードは
...3...1...7...
となる。
ここで、xの先にyが来ることを(x,y)と表すと、上記は以下で表せる。
[(3,1),(3,7),(1,7)]

テキストをこの形式に変換すると以下。
[(1,0),(1,2),(1,6),(1,8),(1,9),(2,0),(2,8),(2,9),(3,0),(3,1),(3,2),(3,6),(3,8),(3,9),(6,0),(6,2),(6,8),(6,9),(7,0),(7,1),(7,2),(7,3),(7,6),(7,8),(7,9),(8,0),(8,9),(9,0)]

有向グラフで眺めたいので、graphvizのdot形式で表現する。

```dot
digraph sample {
  1 -> 0;
  ...
}
```

graphvizに食わせる。

```sh
dot -Tsvg data/079-graph.dot -o data/079-graph.svg
```

画像を眺めると、上から下に数をつなげれば答えが出る。
-}

import           Data.Char (digitToInt)
import           Data.List (nub, sort)

main = do
  txt<- readFile "data/p079_keylog.txt"
  let ds = (nub . sort . concatMap dirs . lines) txt
  putStrLn $ toDot ds

-- 1つの試行を、3つのベクトルに変換
dirs :: String -> [(Int,Int)]
dirs = (\[x,y,z] -> [(x,y),(x,z),(y,z)]) . map digitToInt

-- 有向グラフをgraphvizに食わせる形式に変換
toDot :: Show a => [(a,a)] -> String
toDot = (++ suffix) . (prefix ++) . toStr
  where
    toStr = concatMap (\(x,y) -> "  " ++ show x ++ " -> " ++ show y ++ ";\n")
    prefix = "digraph sample {\n"
    suffix = "}\n"

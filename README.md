# projecteuler-hs-jp

[![Build Status](https://travis-ci.org/yuto-matsum/projecteuler-hs-jp.svg?branch=master)](https://travis-ci.org/yuto-matsum/projecteuler-hs-jp)
[![CC 2.0 BY-NC-SA](https://i.creativecommons.org/l/by-nc-sa/2.0/jp/80x15.png)](http://creativecommons.org/licenses/by-nc-sa/2.0/jp/)

[Project Euler](https://projecteuler.net/)の問題に対して、Haskellと日本語で試行錯誤する。

問題の日本語訳は、[PukiWiki](http://odz.sakura.ne.jp/projecteuler/)に全面的にお世話になっております。
そのため、ライセンスはPukiWiki同様に[クリエイティブ・コモンズの表示 - 非営利 - 継承 2.0](http://creativecommons.org/licenses/by-nc-sa/2.0/jp/)です。

## ビルド方法

Stackがインストールされていることが前提。

問題1(`src/001.lhs`)は、以下の方法でビルド・実行できる。

```sh
git clone https://github.com/yuto-matsum/projecteuler-hs-jp.git
cd projecteuler-hs-jp
stack build :001
stack exec 001
```

問題1をロードしてGHCiを起動したいなら

```sh
stack ghci :001
```

# Game 27 AI

Game27のAI

Python版: https://github.com/wass88/game-27-python

# Build Docker Image
`docker build . -t game-27-ai`


## Rule of Game

出典: https://shop.neu-icarus.com/items/13455796

先手O、後手Xの2人ゲームである。
初期状態は9つのコマの塔がボードの両端に立った次の状態である。
ここで左側を塔の上部とする。

```
0:OOOOOOOOO
1:
2:
3:
4:
5:
6:
7:
8:XXXXXXXXX
```

手番では自分の塔の上部を好きな数取って、自分の塔の数だけすすめる。
自分の塔とは、塔の最上部が自分のコマである塔のことを言う。
先手は9へ向かって、後手は0へ向かってすすめる。

例えば先手が `0` から　`5` つ動かすと次のようになる。
```
0:     OOOO
1:    OOOOO
2:
3:
4:
5:
6:
7:
8:XXXXXXXXX
```

例えば先手が `8` から　`8` つ動かすと次のようになる。
```
0:     OOOO
1:    OOOOO
2:
3:
4:
5:
6:
7: XXXXXXXX
8:        X
```

動かせる手がないときのみパスをする。
両者がパスとなる局面でゲームが終了する。

先手は8にある両者のコマの数。後手は0にある両者のコマの数が得点となる。

得点の高い方の勝ちである。

次の局面は終了局面の例である。先手が7点、後手が3点である。
先手は2つすすめられる塔がなく、後手は3つすすめられる塔がないため、終了である。

```
0:     XXX
1:      XO
2:    XOXX
3:
4:
5:
6:
7:      OX
8: OOOOOOX
```

## Protocol

標準入出力をするプログラムを作る。
`<-` は入力を表し、 `->` は出力を表す記号であり、実際には入出力しない。
stderr は無視される (見えるようにする予定)

後手の入出力例:

```
<- init 1 5
<- played move 0 2
<- wait
-> move 8 3
<- played move 0 2
<- wait
-> move 7 1
以下同様に続く…
<- result 0
```

- <- `init (0|1) \d+`: 0は先手 1は後手 (未実装: 次の数字は1手あたりのタイムアウト秒)
- <- `played (move \d \d+|pass)`: 相手の手
- <- `wait`: 入力待ち。`wait`が入力された後、自分の手を出力する、
- -> `(move \d+ \d+|pass)`: 自分の手を出力する。どの塔からいくつ動かすか。
- <- `result \d+`: あなたの結果。塔の差分 (-18 ~ 18)

# 実際の対局の挙動の確認のためのバイナリ

手元で試合を確認したい場合は、playoutコマンドを活用してください。

https://github.com/wass88/game-ai/release

```
$./playout game27 "先手コマンド" "後手コマンド"
```
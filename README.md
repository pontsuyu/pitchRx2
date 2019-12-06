
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pitchRx2

<img src="pitchRx2_resize.png" align="right"> これは
[pitchRxパッケージ](https://github.com/cpsievert/pitchRx)
を基にして作成しています。

## インストール

`devtools::install_github("pontsuyu/pitchRx2")`

## 概要

パッケージに含まれる関数は以下の通りです。

  - `get_gids`<br> 指定した年のすべてのgame\_idを取得する
  - `scrape_game_info`<br> 試合ごとの選手や審判などの情報をリストに格納する
  - `scrape_inning_all`<br> 投球、打席結果、ランナー状況、牽制、選手交代のデータをsqlite3に格納する
  - `get_snapshots`<br>
    `scrape_inning_all`で得た投球結果から、リリースからホームベース通過までの軌道を計算し、3次元データとして返す
  - `get_strikezones`<br> 打者の身長からストライクゾーンを計算する
  - `animateFX`<br>
    `scrape_inning_all`で得た投球情報を`ggplot2`で可視化し、GIF画像として保存する。


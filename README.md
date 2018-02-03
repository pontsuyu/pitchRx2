
<!-- README.md is generated from README.Rmd. Please edit that file -->
pitchRx2
========

<img src="pitchRx2_resize.png" align="right"> これは[pitchRxパッケージ](https://github.com/cpsievert/pitchRx)を基にして作成しています。

インストール
------------

`devtools::install_github("pontsuyu/pitchRx2")`

概要
----

パッケージに含まれる関数は以下の通りです。

-   `get_gids`<br> 指定した年のすべてのgame\_idを取得する
-   `scrape_inning_all`<br> 投球、打席結果、ランナー状況、牽制、選手交代のデータをsqlite3に格納する
-   `getSnapshots`<br> `scrape_inning_all`で得た投球結果から、リリースからホームベース通過までの軌道を計算し、3次元データとして返す
-   `getStrikezones`<br> 打者の身長からストライクゾーンを計算する
-   `animateFX`<br> `scrape_inning_all`で得た投球情報を`ggplot2`で可視化する。`animation::saveHTML`や`animation::saveGIF`と併用して使う

紹介
----

この`pitchRx2`を使った集計を[コチラ](https://pontsuyu.github.io/tsuyulog/post/2018/02/03/pitchf/x%E3%81%8B%E3%82%89%E8%A6%8B%E3%81%9F%E7%94%B0%E4%B8%AD%E5%B0%86%E5%A4%A7%E6%8A%95%E6%89%8B%EF%BC%91/)に載せています。

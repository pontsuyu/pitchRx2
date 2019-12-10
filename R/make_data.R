#' make data which is easy to use
#'
#' @param db_sqlite sqlite file.
#' @param player data.frame which has id and name column.
#'
#' @return data.frame
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
#' @examples
#' make_data("mlb2018.sqlite")
#'
make_data <- function(db_sqlite, player) {
  db <- src_sqlite(db_sqlite)
  # id-name data
  if (missing(player)) {
    player <- pitchRx2::players$player %>%
      select(id, first, last) %>%
      unite("name", first, last, sep = " ") %>%
      distinct() %>%
      mutate(tmp = row_number()) %>%
      arrange(desc(tmp)) %>%
      group_by(id) %>%
      mutate(id_order = row_number()) %>%
      filter(id_order == 1) %>%
      select(-tmp, -id_order) %>%
      ungroup() %>%
      mutate(id = as.numeric(id))
  }

  dat <- tbl(db, sql("SELECT
                  pitcher, -- 投手ID
                  p_throws, -- 投手の利き腕
                  batter, -- 打者ID
                  stand, -- 打者の立ち位置
                  pitch_type, -- 球種
                  start_speed, -- 初速
                  end_speed, -- 終速
                  spin_rate, -- 回転数
                  spin_dir, -- 回転軸
                  px, pz, -- 投球ロケーション
                  x0, y0, z0, -- リリースポイント
                  vx0, vy0, vz0, -- 速度
                  ax, ay, az, -- 加速度
                  b_height, -- バッターの身長(feet-inch)
                  break_angle, -- 変化角
                  break_length, -- 変化量
                  zone, -- ゾーン
                  pit.num as event_num, -- 試合ごとのイベント番号
                  pit.des, -- 投球結果
                  type,  -- 簡易投球結果
                  event, -- 打席結果
                  atb.date, -- 日時
                  pit.url -- URL
                  FROM
                  atbat atb, -- 打席テーブル
                  pitch pit -- 投球データテーブル
                  WHERE atb.url = pit.url -- スクレイピング先url
                  AND atb.inning = pit.inning -- イニング
                  AND atb.inning_side = pit.inning_side -- 表/裏
                  AND atb.gameday_link = pit.gameday_link -- gameid
                  AND atb.next_ = pit.next_ -- 次打者の有無
                  AND atb.num = pit.num -- イベント番号
                   ")) %>%
    as.data.frame() %>%
    left_join(player, by = c("pitcher" = "id")) %>%
    rename(pitcher_name = name) %>%
    left_join(player, by = c("batter" = "id")) %>%
    rename(batter_name = name)

  return(dat)
}

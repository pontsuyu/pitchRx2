library(pitchRx)
library(tidyverse)
library(DBI)
db <- src_sqlite("sff.sqlite3", create = T)
# files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
system.time({
  scrape(start = "2017-06-01", end = "2017-06-30", connect = db$con)
})
db_list_tables(db$con)
# [1] "action" "atbat"  "coach"  "game"   "hip"    "media"  "pitch"
# [8] "player" "po"     "runner" "umpire"
pitch <- dbGetQuery(db$con, "SELECT * FROM pitch")
atbat <- dbGetQuery(db$con, "SELECT * FROM atbat")
notneed <- c("code", "event_num", "play_guid", "end_tfs_zulu",
             "event_num", "event_es", "play_guid", "event2_es")
atbat <- atbat %>% select(-(29:33))
unique(atbat$pitcher_name)
dat <- inner_join(pitch, atbat) %>% filter(pitcher_name=="Masahiro Tanaka")
x <- list(
  facet_grid(pitcher_name ~ stand, labeller = label_both),
  theme_bw(),
  coord_equal()
)
interactiveFX(dat[dat$pitch_type=="FF"|dat$pitch_type=="SL"|dat$pitch_type=="FS",], spheres = F)
strikeFX(dat, geom = "tile") +
  # facet_grid(pitcher_name ~ stand) +
  coord_equal() +
  theme_bw() +
  viridis::scale_fill_viridis()


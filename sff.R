library(pitchRx)
library(tidyverse)
library(DBI)
db <- src_sqlite("sff.sqlite3", create = T)
# files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
nya <- game_ids %>% str_subset("2017.*nya.*")
system.time({
  scrape(game.ids = nya, connect = db$con)
})
db_list_tables(db$con)
# [1] "action" "atbat" "pitch" "po" "runner"
pitch <- dbGetQuery(db$con, "SELECT * FROM pitch")
atbat <- dbGetQuery(db$con, "SELECT * FROM atbat")
notneed <- c("code", "event_num", "play_guid", "end_tfs_zulu",
             "event_num", "event_es", "play_guid", "event2_es")
atbat <- atbat %>% select(-(29:33))
sort(unique(atbat$pitcher_name))
dat <- inner_join(pitch, atbat) %>% filter(pitcher_name=="Masahiro Tanaka")
x <- list(
  facet_grid(pitcher_name ~ stand, labeller = label_both),
  coord_equal()
)
saveHTML({ animateFX(dat, layers = x,point.alpha = 0.6)}, interval = 0.1,
         verbose = FALSE, ani.height = 600, ani.width = 800)
interactiveFX(dat[dat$pitch_type=="FF"|dat$pitch_type=="SL"|dat$pitch_type=="FS",], spheres = F)
strikeFX(dat, geom = "tile") +
  # facet_grid(pitcher_name ~ stand) +
  coord_equal() +
  theme_bw() +
  viridis::scale_fill_viridis()


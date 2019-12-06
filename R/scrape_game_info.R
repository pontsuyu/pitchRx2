#' Scrape MLB Players Data
#'
#' Function for obtaining MLB Players Data.
#' \href{http://gd2.mlb.com/components/game/mlb/year_2019/month_04/day_04/gid_2019_04_04_wasmlb_nynmlb_1/players.xml}{players.xml}
#'
#' @param gids Gameday URLs vector.
#' @return list
#'
#' @importFrom xml2 read_html
#' @importFrom XML xmlParse
#' @import XML2R
#' @import dplyr
#'
#' @export
#'
#' @examples
#' data(game_ids, package = "pitchRx2")
#' gid <- str_subset(game_ids, "^gid_2019_04_05_")
#' scrape_game_info(gid)
scrape_game_info <- function(gids) {
  # make URLs
  URLs <- paste0(makeUrls(gids), "/players.xml")

  # scrape player's data
  docs <- sapply(URLs, function(URL) {
    text <- try(read_html(URL), silent = T)
    if (class(text)[1] != "try-error") {
      xmlParse(text, asText = TRUE)
    }
  })
  tables <- docsToNodes(docs, "/") %>%
    nodesToList() %>%
    listsToObs(urls = URLs, url.map = FALSE) %>%
    collapse_obs()

  tbl_name <- names(tables)
  for (i in 1:length(tbl_name)) {
    name <- unlist(strsplit(tbl_name[i], "//"))
    tables[[i]] <- as.data.frame(tables[[i]], stringsAsFactors = F)
    names(tables)[i] <- name[length(name)]
  }

  tables_res <- list(game = tables$game,
                     team = tables$team,
                     coach = tables$coach,
                     player = tables$player,
                     umpire = tables$umpire)
  return(tables_res)
}

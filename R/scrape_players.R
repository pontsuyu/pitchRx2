#' Scrape MLB Players Data
#'
#' Function for obtaining MLB Players Data.
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml}
#'
#' @param gids Gameday URLs vector.
#' @param db_name sqlite3 file name which you wanna make.
#' @return Nothing
#'
#' @importFrom xml2 read_html
#' @importFrom XML xmlParse
#' @import dplyr
#' @import foreach
#' @import XML2R
#'
#' @export
#'
#' @examples
#' data(game_ids, package = "pitchRx2")
#' gid <- str_subset(game_ids, "^gid_2017_04_05_")
#' scrape_players(gid, "Players")
#'
scrape_players <- function(gids, db_name) {

    fn <- paste0(db_name, ".sqlite3")
    if(file.exists(fn)) file.remove(fn)
    # make data-base file
    db <- src_sqlite(fn, create = T)

    # make URLs
    URLs <- paste0(makeUrls(gids), "/players.xml")

    # scrape player's data
    docs <- foreach(URL = URLs) %do% {
        text <- try(read_html(URL), silent = T)
        if(class(text)[1] != "try-error"){
            xmlParse(text, asText = TRUE)
        }
    }
    obs <- docsToNodes(docs, "/") %>%
        nodesToList() %>%
        listsToObs(urls = URLs, url.map = FALSE)
    tables <- collapse_obs(obs)
    for(i in names(tables)){
        name <- unlist(strsplit(i, "//"))
        copy_to(db, as.data.frame(tables[[i]]),
                name = name[length(name)],
                temporary = FALSE,
                overwrite = TRUE)
    }
}

# wrapper around collapse_obs to ensure a list is always returned
# collapse_obs2 <- function(x) {
#     val <- XML2R::collapse_obs(x)
#     if (!is.list(val)) {
#         val <- list(val)
#         names(val) <- unique(names(x))
#     }
#     return(val)
# }

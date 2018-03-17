#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. This function currently has support for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml}
#'
#' @param gid character vector of gameday_links.
#' @param db_name sqlite3 file name.
#' @return Nothing
#' @export
#' @examples
#' data(game_ids, package = "pitchRx2")
#' gid <- str_subset(game_ids, "^gid_2017_04_05_")
#' scrape_players(gid, "Gameday")
#'
scrape_players <- function(gid, db_name) {
    # make data-base file
    db <- try(dplyr::src_sqlite(paste0(db_name, ".sqlite3"), create = F), silent = T)
    if(class(db)[1] == "try-error") db <- dplyr::src_sqlite(paste0(db_name, ".sqlite3"), create = T)
    # Now scrape the inning/inning_all.xml files
    players.files <- paste0(makeUrls(gid), "/players.xml")
    n.files <- length(players.files)
    # grab subset of files to be parsed
    docs <- foreach::foreach(i = seq_len(n.files)) %do% {
        text <- try(xml2::read_html(players.files[i]), silent = T)
        if(class(text)[1] != "try-error") XML::xmlParse(text, asText = TRUE)
    }
    nodes <- XML2R::docsToNodes(docs, "/")
    l <- XML2R::nodesToList(nodes)
    obs <- XML2R::listsToObs(l, urls = players.files, url.map = FALSE)
    tables <- collapse_obs2(obs)
    # Free up some memory
    rm(obs)
    gc();gc()
    for (i in names(tables)) {
        name <- unlist(strsplit(i, "//"))
        dplyr::copy_to(db, as.data.frame(tables[[i]]),
                       name = name[length(name)], temporary = FALSE,
                       overwrite = TRUE, append = TRUE)
    }
}

# Make Gameday urls
makeUrls <- function(x) {
    root <- "http://gd2.mlb.com/components/game/"
    league <- rep("mlb", length(x))
    not.mlb <- !grepl("mlb", x)
    # If 'mlb' does not appear in the gid, use the home team's league
    if (any(not.mlb)) league[not.mlb] <- substr(x[not.mlb], 26, 28)
    base <- paste0(root, league)
    paste0(base, "/year_", substr(x, 5, 8),
           "/month_", substr(x, 10, 11), "/day_", substr(x, 13, 14), "/", x)
}

# wrapper around collapse_obs to ensure a list is always returned (if only table, return list of length 1)
collapse_obs2 <- function(x) {
    val <- XML2R::collapse_obs(x)
    if (!is.list(val)) {
        val <- list(val)
        names(val) <- unique(names(x))
    }
    return(val)
}

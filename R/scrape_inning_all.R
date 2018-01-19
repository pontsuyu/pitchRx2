#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. \code{scrape} currently has support for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_hit.xml}{inning/inning_hit.xml},
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/players.xml}{players.xml}, or
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/miniscoreboard.xml}{miniscoreboard.xml}.
#' It's worth noting that PITCHf/x is contained in files ending with "inning/inning_all.xml", but the other files can complement this data depending on the goal for analysis.
#' Any collection of file names may be passed to the \code{suffix} argument, and \code{scrape} will retrieve data from a (possibly large number)
#' of files based on either a window of dates or a set of \code{game.ids}.
#' If collecting data in bulk, it is strongly recommended that one establishes a database connection and supplies the
#' connection to the \code{connect} argument. See the examples section for a simple example of how to do so.
#'
#' @note This function was adapted from \code{scrapeFX} which is deprecated as of version 1.0
#' @param start character string specifying a date "yyyy-mm-dd" to commence scraping.
#' @param end character string specifying a date "yyyy-mm-dd" to terminate scraping.
#' @param game.ids character vector of gameday_links. If this option is used, \code{start} and \code{end} are ignored.
#' See \code{data(gids, package="pitchRx")} for examples.
#' @param suffix character vector with suffix of the XML files to be parsed. Currently supported options are:
#' 'players.xml', 'miniscoreboard.xml', 'inning/inning_all.xml', 'inning/inning_hit.xml'.
#' @param connect A database connection object. The class of the object should be "MySQLConnection" or "SQLiteConnection".
#' If a valid connection is supplied, tables will be copied to the database, which will result in better memory management.
#' If a connection is supplied, but the connection fails for some reason, csv files will be written to the working directory.
#' @param ... arguments passed onto \code{XML2R::XML2Obs}. Among other things, this can be used to switch on asynchronous downloads.
#' @seealso If you want to add support for more file types, the \code{XML2R} package is a good place to start.
#' @return Returns a list of data frames (or nothing if writing to a database).
#' @export
#' @import XML2R
#' @examples
#' \dontrun{
#' # Collect PITCHf/x (and other data from inning_all.xml files) from
#' # all games played on August 1st, 2013 (using asynchronous downloads)
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01")
#' #As of XML2R 0.0.5, asyncronous downloads can be performed
#' dat <- scrape(start = "2013-08-01", end = "2013-08-01", async = TRUE)
#'
#' # Scrape PITCHf/x from Minnesota Twins 2011 season
#' data(gids, package = "pitchRx")
#' twins11 <- gids[grepl("min", gids) & grepl("2011", gids)]
#' dat <- scrape(game.ids = twins11[1]) #scrapes 1st game only
#'
#' data(nonMLBgids, package = "pitchRx")
#' # Grab IDs for triple A games on June 1st, 2011
#' # This post explains more about obtaining game IDs with regular expressions --
#' # http://baseballwithr.wordpress.com/2014/06/30/pitchrx-meet-openwar-4/
#' aaa <- nonMLBgids[grepl("2011_06_01_[a-z]{3}aaa_[a-z]{3}aaa", nonMLBgids)]
#' dat <- scrape(game.ids = aaa)
#'
#' # Create SQLite database, then collect and store data in that database
#' library(dplyr)
#' my_db <- src_sqlite("Gameday.sqlite3")
#' scrape(start = "2013-08-01", end = "2013-08-01", connect = my_db$con)
#'
#' # Collect other data complementary to PITCHf/x and store in database
#' files <- c("inning/inning_hit.xml", "miniscoreboard.xml", "players.xml")
#' scrape(start = "2013-08-01", end = "2013-08-01", connect=my_db$con, suffix = files)
#'
#' # Simple example to demonstrate database query using dplyr
#' # Note that 'num' and 'gameday_link' together make a key that allows us to join these tables
#' locations <- select(tbl(my_db, "pitch"), px, pz, des, num, gameday_link)
#' names <- select(tbl(my_db, "atbat"), pitcher_name, batter_name, num, gameday_link)
#' que <- inner_join(locations, filter(names, batter_name == "Paul Goldschmidt"),
#'                    by = c("num", "gameday_link"))
#' que$query #refine sql query if you'd like
#' pitchfx <- collect(que) #submit query and bring data into R
#'
#' }
#'
scrape_inning_all <- function(gid, db_name) {
    # make data-base file
    db <- try(dplyr::src_sqlite(paste0(db_name, ".sqlite3")), silent = T)
    if(class(db)[1] == "try-error")
        db <- dplyr::src_sqlite(paste0(db_name, ".sqlite3"), create = T)
    # gid check
    if (!all(gid %in% pitchRx2::game_ids))
        stop("Any Game IDs supplied to the gids option should be of the form gid_YYYY_MM_DD_xxxmlb_zzzmlb_1")

    # Now scrape the inning/inning_all.xml files
    inning.files <- paste0(makeUrls(gid), "/inning/inning_all.xml")
    n.files <- length(inning.files)
    #grab subset of files to be parsed
    docs <- foreach::foreach(i = seq_len(length(urls))) %do% {
        text <- try(xml2::read_html(urls[i]), silent = T)
        if(class(text)[1] != "try-error") XML::xmlParse(text, asText = TRUE)
    }
    nodes <- XML2R::docsToNodes(docs, "/")
    l <- XML2R::nodesToList(nodes)
    obs <- XML2R::listsToObs(l, urls = urls, url.map = FALSE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//pitch", "html//body//game//inning//bottom//atbat//pitch"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//runner", "html//body//game//inning//bottom//atbat//runner"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//po", "html//body//game//inning//bottom//atbat//po"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat", "html//body//game//inning//bottom//atbat"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//action", "html//body//game//inning//bottom//action"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::add_key(obs, parent="html//body//game//inning", recycle="num", key.name="inning", quiet=TRUE)
    obs <- XML2R::add_key(obs, parent="html//body//game//inning", recycle="next", key.name="next_", quiet=TRUE)
    names(obs) <- sub("^html//body//game//inning//action$", "html//body//game//inning//atbat//action", names(obs))
    obs <- XML2R::add_key(obs, parent="html//body//game//inning//atbat", recycle="num", quiet=TRUE)
    #no longer need the 'game' and 'game//inning' observations
    nms <- names(obs)
    rm.idx <- c(grep("^html//body//game$", nms), grep("^html//body//game//inning$", nms))
    if (length(rm.idx) > 0) obs <- obs[-rm.idx]
    tables <- collapse_obs2(obs)
    #Free up some memory
    rm(obs)
    gc();gc()
    #simplify table names
    tab.nms <- names(tables)
    tab.nms <- sub("^html//body//game//inning//atbat$", "atbat", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//action$", "action", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//po$", "po", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//runner$", "runner", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//pitch$", "pitch", tab.nms)
    tables <- setNames(tables, tab.nms)
    #Add names to atbat table for convenience
    data(players, package="pitchRx2")
    players$id <- as.character(players$id)
    #Add batter name to 'atbat'
    colnames(tables[["atbat"]]) <- sub("^batter$", "id", colnames(tables[["atbat"]]))
    tables[["atbat"]] <- dplyr::left_join(as.data.frame(tables[["atbat"]], stringsAsFactors = FALSE), players, by = "id")
    colnames(tables[["atbat"]]) <- sub("^id$", "batter", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^full_name$", "batter_name", colnames(tables[["atbat"]]))
    #Add pitcher name to 'atbat'
    colnames(tables[["atbat"]]) <- sub("^pitcher$", "id", colnames(tables[["atbat"]]))
    tables[["atbat"]] <- dplyr::left_join(as.data.frame(tables[["atbat"]], stringsAsFactors = FALSE), players, by = "id")
    colnames(tables[["atbat"]]) <- sub("^id$", "pitcher", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^full_name$", "pitcher_name", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^des", "atbat_des", colnames(tables[["atbat"]]))
    # Coerce matrices to data frames; turn appropriate variables into numerics
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    #generate a "count" column from "b" (balls) & "s" (strikes)
    tables[["pitch"]] <- appendPitchCount(tables[["pitch"]])
    tables[["atbat"]] <- appendDate(tables[["atbat"]])

    #Try to write tables to database, if that fails, write to csv. Then clear up memory
    for (i in names(tables)){
        value <- tables[[i]]
        # '.' in table names are not good!
        names(value) <- sub("\\.", "_", names(value))
        # if url.map=FALSE, have to change 'url_key' to url
        names(value) <- sub("^url_key$", "url", names(value))
        # url should never be NA -- this is specific to pitchRx implementation
        if ("url" %in% colnames(value)) {
            throw <- is.na(value$url)
            if (any(throw)) value <- value[-throw,]
        }
        dplyr::copy_to(db, value, name = i, temporary = FALSE, overwrite = TRUE, append = TRUE)
    }
}

#' Construct Gameday urls based on some parameters.
#'
#' This is a convenience function (used by \link{scrape}) which constructs urls with the common
#' Gameday root \url{http://gd2.mlb.com/components/game/mlb/}.
#'
#' @param start date "yyyy-mm-dd" to commence scraping.
#' @param end date "yyyy-mm-dd" to terminate scraping.
#' @param gids The default value "infer" suggests gameday_links should be derived
#' and appended appropriately (based on values of \code{start} and \code{end}).
#' Otherwise, a character vector with gameday_links can be supplied.
#' @return Returns a character vector.
#' @export
#' @examples
#'
#' # XML file names with pitch-by-pitch level data
#' prefix <- makeUrls(start="2011-04-04", end="2011-04-04")
#' paste0(prefix, "/inning/inning_all.xml")
#' # XML file names with hit location data
#' paste0(prefix, "/inning/inning_hit.xml")
#' # XML file names with game-by-game level data
#' paste0(makeUrls(start="2011-04-04", end="2011-04-04", gids=""), "/miniscoreboard.xml")
#' # Use gids option instead
#' data(gids)
#' identical(prefix, makeUrls(gids=gids[grep("2011_04_04", gids)]))
#'
makeUrls <- function(x) {
    root <- "http://gd2.mlb.com/components/game/"
    # Assume the league is 'mlb' unless we find evidence otherwise
    league <- rep("mlb", length(x))
    not.mlb <- !grepl("mlb", x)
    # If 'mlb' does not appear in the gid, use the home team's league
    if (any(not.mlb)) league[not.mlb] <- substr(x[not.mlb], 26, 28)
    base <- paste0(root, league)
    paste0(base,
           "/year_", substr(x, 5, 8),
           "/month_", substr(x, 10, 11),
           "/day_", substr(x, 13, 14), "/", x)
}

#wrapper around collapse_obs to ensure a list is always returned (if only table, return list of length 1)
collapse_obs2 <- function(x) {
    val <- XML2R::collapse_obs(x)
    if (is.list(val)) {
        return(val)
    } else {
        li <- list(val)
        names(li) <- unique(names(x))
        return(li)
    }
}

# Take a matrix and turn into data frame and turn relevant columns into numerics
format.table <- function(dat, name) {
    switch(name,
           game = nums <- c("venue_id", "scheduled_innings", "away_team_id", "away_league_id", "home_team_id",
                            "home_league_id", "away_games_back_wildcard", "away_win",  "away_loss", "home_win",
                            "home_loss", "inning", "outs", "away_team_runs","home_team_runs", "away_team_hits",
                            "home_team_hits", "away_team_errors", "home_team_errors"),
           player = nums <- c("id", "num", "avg", "hr", "rbi", "bat_order", "wins", "losses", "era"),
           coach = nums <- c("id", "num"),
           umpire = nums <- "id",
           hip = nums <- c("x", "y", "batter", "pitcher", "inning"),
           action = nums <- c("b", "s", "o", "player", "pitch", "inning"),
           atbat = nums <- c("pitcher", "batter", "num", "b", "s", "o", "inning"),
           pitch = nums <- c("id", "x", "y", "start_speed", "end_speed", "sz_top", "sz_bot", "pfx_x", "pfx_z", "px",
                             "pz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az", "type_confidence",
                             "zone", "nasty", "spin_dir", "spin_rate", "inning", "num", "on_1b", "on_2b", "on_3b"),
           po = nums <- c("inning", "num"),
           runner = nums <- c("id", "inning", "num"))
    #For some reason, records are sometimes duplicated, remove them!
    dat <- data.frame(dat[!duplicated(dat),], stringsAsFactors=FALSE)
    numz <- nums[nums %in% names(dat)] #error handling (just in case one of the columns doesn't exist)
    for (i in numz) dat[, i] <- suppressWarnings(as.numeric(dat[, i]))
    if ("game" %in% name) {
        dat$url_scoreboard <- dat$url
        dat$url <- paste0(gsub("miniscoreboard.xml", "", dat$url), "gid_", dat$gameday_link, "/inning/inning_all.xml")
        # These fields only show up for suspended games...I don't think they're worth tracking...
        dat <- dat[, !names(dat) %in% c("runner_on_base_status", "runner_on_1b")]
    } else { #create a 'gameday_link' column for easier linking of tables
        if (length(grep("^url$", names(dat)))) dat$gameday_link <- sub("/.*", "", sub(".*gid", "gid", dat$url))
    }
    return(dat)
}


# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendPitchCount <- function(dat) {
    if (any(!c("type", "gameday_link", "num") %in% colnames(dat))){
        warning("Count column couldn't be created")
        return(dat)
    }
    balls <- as.numeric(dat[,"type"] %in% "B")
    strikes <- as.numeric(dat[,"type"] %in% "S")
    pre.idx <- paste(dat[,"gameday_link"], dat[,"num"])
    idx <- factor(pre.idx, levels=unique(pre.idx))
    cum.balls <- unlist(tapply(balls, INDEX=idx, function(x){ n <- length(x); pmin(cumsum(c(0, x[-n])), 3) }))
    cum.strikes <- unlist(tapply(strikes, INDEX=idx, function(x) { n <- length(x); pmin(cumsum(c(0, x[-n])), 2) }))
    count <- paste(cum.balls, cum.strikes, sep = "-")
    return(cbind(dat, count))
}

# Add columns with relevant pitch count to the 'pitch' table.
# @param dat 'pitch' matrix/df
# @return returns the original matrix/df with the proper pitch count column appended.
appendDate <- function(dat) {
    if (!("gameday_link" %in% colnames(dat))){
        warning("'date' column couldn't be created")
        return(dat)
    }
    return(cbind(dat, date = substr(dat[, "gameday_link"], 5, 14)))
}



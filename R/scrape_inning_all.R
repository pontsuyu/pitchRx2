#' Scrape Major League Baseball's Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data. This function currently has support for files ending with:
#' \href{http://gd2.mlb.com/components/game/mlb/year_2011/month_04/day_04/gid_2011_04_04_minmlb_nyamlb_1/inning/inning_all.xml}{inning/inning_all.xml}
#'
#' @param gid character vector of gameday_links.
#' @param db_name sqlite3 file name.
#' @return Nothing
#' @export
#' @examples
#' data(game_ids, package = "pitchRx2")
#' gid <- str_subset(game_ids, "^gid_2017_04_05_")
#' scrape_inning_all(gid, "Gameday")
#'
scrape_inning_all <- function(gid, db_name) {
    # make data-base file
    db <- try(dplyr::src_sqlite(paste0(db_name, ".sqlite3"), create = F, silent = T))
    if(class(db)[1] == "try-error") db <- dplyr::src_sqlite(paste0(db_name, ".sqlite3"), create = T)
    # Now scrape the inning/inning_all.xml files
    inning.files <- paste0(makeUrls(gid), "/inning/inning_all.xml")
    n.files <- length(inning.files)
    # grab subset of files to be parsed
    docs <- foreach::foreach(i = seq_len(n.files)) %do% {
        text <- try(xml2::read_html(inning.files[i]), silent = T)
        if(class(text)[1] != "try-error") XML::xmlParse(text, asText = TRUE)
    }
    nodes <- XML2R::docsToNodes(docs, "/")
    l <- XML2R::nodesToList(nodes)
    obs <- XML2R::listsToObs(l, urls = inning.files, url.map = FALSE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//pitch", "html//body//game//inning//bottom//atbat//pitch"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//runner", "html//body//game//inning//bottom//atbat//runner"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat//po", "html//body//game//inning//bottom//atbat//po"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//atbat", "html//body//game//inning//bottom//atbat"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::re_name(obs, equiv=c("html//body//game//inning//top//action", "html//body//game//inning//bottom//action"), diff.name="inning_side", quiet=TRUE)
    obs <- XML2R::add_key(obs, parent="html//body//game//inning", recycle="num", key.name="inning", quiet=TRUE)
    obs <- XML2R::add_key(obs, parent="html//body//game//inning", recycle="next", key.name="next_", quiet=TRUE)
    names(obs) <- sub("^html//body//game//inning//action$", "html//body//game//inning//atbat//action", names(obs))
    obs <- XML2R::add_key(obs, parent="html//body//game//inning//atbat", recycle="num", quiet=TRUE)
    # no longer need the 'game' and 'game//inning' observations
    nms <- names(obs)
    rm.idx <- c(grep("^html//body//game$", nms), grep("^html//body//game//inning$", nms))
    if (length(rm.idx) > 0) obs <- obs[-rm.idx]
    tables <- collapse_obs2(obs)
    # Free up some memory
    rm(obs)
    gc();gc()
    # simplify table names
    tab.nms <- names(tables)
    tab.nms <- sub("^html//body//game//inning//atbat$", "atbat", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//action$", "action", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//po$", "po", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//runner$", "runner", tab.nms)
    tab.nms <- sub("^html//body//game//inning//atbat//pitch$", "pitch", tab.nms)
    tables <- setNames(tables, tab.nms)
    # Add names to atbat table for convenience
    data(players, package="pitchRx2")
    players$id <- as.character(players$id)
    # Add batter name to 'atbat'
    colnames(tables[["atbat"]]) <- sub("^batter$", "id", colnames(tables[["atbat"]]))
    tables[["atbat"]] <- dplyr::left_join(as.data.frame(tables[["atbat"]], stringsAsFactors = FALSE), players, by = "id")
    colnames(tables[["atbat"]]) <- sub("^id$", "batter", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^full_name$", "batter_name", colnames(tables[["atbat"]]))
    # Add pitcher name to 'atbat'
    colnames(tables[["atbat"]]) <- sub("^pitcher$", "id", colnames(tables[["atbat"]]))
    tables[["atbat"]] <- dplyr::left_join(as.data.frame(tables[["atbat"]], stringsAsFactors = FALSE), players, by = "id")
    colnames(tables[["atbat"]]) <- sub("^id$", "pitcher", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^full_name$", "pitcher_name", colnames(tables[["atbat"]]))
    colnames(tables[["atbat"]]) <- sub("^des", "atbat_des", colnames(tables[["atbat"]]))
    # Coerce matrices to data frames; turn appropriate variables into numerics
    for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name=i)
    # generate a "count" column from "b" (balls) & "s" (strikes)
    tables[["pitch"]] <- appendPitchCount(tables[["pitch"]])
    tables[["atbat"]] <- appendDate(tables[["atbat"]])

    for (i in names(tables)){
        value <- tables[[i]]
        # '.' in table names are not good!
        names(value) <- sub("\\.", "_", names(value))
        names(value) <- sub("^url_key$", "url", names(value))
        if ("url" %in% colnames(value)) value <- value[!is.na(value$url),]
        dplyr::copy_to(db, value, name = i, temporary = FALSE, overwrite = TRUE, append = TRUE)
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
appendDate <- function(dat) {
    if (!("gameday_link" %in% colnames(dat))){
        warning("'date' column couldn't be created")
        return(dat)
    }
    return(cbind(dat, date = substr(dat[, "gameday_link"], 5, 14)))
}

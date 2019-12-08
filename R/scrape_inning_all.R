#' Scrape MLB Gameday Data
#'
#' Function for obtaining PITCHf/x and other related Gameday Data.
#' \href{http://gd2.mlb.com/components/game/mlb/year_2019/month_04/day_04/gid_2019_04_04_wasmlb_nynmlb_1/inning/inning_all.xml}{inning/inning_all.xml}
#'
#' @param gid character vector. vector of gameday_links.
#' @param db_name generating sqlite3 file name.
#' @return Nothing
#'
#' @importFrom xml2 read_html
#' @importFrom XML xmlParse
#' @importFrom stringr str_detect
#' @import foreach
#' @import doParallel
#' @import XML2R
#' @import dplyr
#'
#' @export
#' @examples
#' data(game_ids, package = "pitchRx2")
#' gid <- str_subset(game_ids, "^gid_2017_04_05_")
#' scrape_inning_all(gid, "Gameday")
scrape_inning_all <- function(gids, db_name = "database") {
  URLs <- paste0(makeUrls(gids), "/inning/inning_all.xml")
  cores <- parallel::detectCores(logical = FALSE)
  cluster <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cluster)
  obs <- foreach(
    i = 1:length(URLs), .combine = c,
    .packages = c("XML2R", "dplyr", "purrr", "xml2")
  ) %dopar% {
    tmp <- try(read_xml(URLs[i]) %>% xml_children())
    if (length(tmp) != 0) {
      valid.urls <- sapply(tmp, function(x) attr(x, "XMLsource"))
      tmp %>%
        map(xmlParse) %>%
        map(xmlChildren) %>%
        nodesToList() %>%
        listsToObs(urls = valid.urls, append.value = TRUE, as.equiv = TRUE, url.map = FALSE) %>%
        map(function(x) cbind(x, url = URLs[i]))
    }
  }
  parallel::stopCluster(cluster)
  suppressWarnings({
    obs <- re_name(obs,
      equiv = c("inning//top//atbat//pitch", "inning//bottom//atbat//pitch"),
      diff.name = "inning_side", quiet = TRUE
    )
    obs <- re_name(obs,
      equiv = c("inning//top//atbat//runner", "inning//bottom//atbat//runner"),
      diff.name = "inning_side", quiet = TRUE
    )
    obs <- re_name(obs,
      equiv = c("inning//top//atbat//po", "inning//bottom//atbat//po"),
      diff.name = "inning_side", quiet = TRUE
    )
    obs <- re_name(obs,
      equiv = c("inning//top//atbat", "inning//bottom//atbat"),
      diff.name = "inning_side", quiet = TRUE
    )
    obs <- re_name(obs,
      equiv = c("inning//top//action", "inning//bottom//action"),
      diff.name = "inning_side", quiet = TRUE
    )
    obs <- add_key(obs,
      parent = "inning", recycle = "num",
      key.name = "inning", quiet = TRUE
    )
    obs <- add_key(obs,
      parent = "inning", recycle = "next",
      key.name = "next_", quiet = TRUE
    )
    names(obs) <- sub("^inning//action$", "inning//atbat//action", names(obs))
    obs <- add_key(obs,
      parent = "inning//atbat",
      recycle = "num", quiet = TRUE
    )
  })
  # no longer need the 'game' and 'inning' observations
  rm.idx <- str_detect(names(obs), "inning$")
  obs <- obs[!rm.idx]
  tables <- collapse_obs(obs)

  # simplify table names
  tab.nms <- names(tables)
  tab.nms <- gsub(".*atbat$", "atbat", tab.nms)
  tab.nms <- gsub(".*action$", "action", tab.nms)
  tab.nms <- gsub(".*po$", "po", tab.nms)
  tab.nms <- gsub(".*runner$", "runner", tab.nms)
  tab.nms <- gsub(".*pitch$", "pitch", tab.nms)
  tables <- setNames(tables, tab.nms)
  tables <- lapply(tables, function(x) as.data.frame(x = x, stringsAsFactors = FALSE))

  colnames(tables$atbat) <- sub("^des", "atbat_des", colnames(tables$atbat))
  for (i in names(tables)) tables[[i]] <- format.table(tables[[i]], name = i)
  # generate a "count" column from "b" (balls) & "s" (strikes)
  tables$pitch <- appendPitchCount(tables$pitch)
  tables$atbat <- appendDate(tables$atbat)

  # make data-base file
  fn <- paste0(db_name, ".sqlite3")
  if (file.exists(fn)) file.remove(fn)
  db <- DBI::dbConnect(RSQLite::SQLite(), dbname = fn)

  for (i in names(tables)) {
    value <- tables[[i]]
    names(value) <- sub("\\.", "_", names(value))
    names(value) <- sub("^url_key$", "url", names(value))
    if ("url" %in% colnames(value)) value <- value[!is.na(value$url), ]
    copy_to(db, value, name = i, temporary = FALSE)
  }
  invisible(db_drop_table(db, "sqlite_stat1"))
  invisible(db_drop_table(db, "sqlite_stat4"))
  DBI::dbDisconnect(db)
}

# Take a matrix and turn into data frame and turn relevant columns into numerics
format.table <- function(dat, name) {
  switch(name,
    action = nums <- c("b", "s", "o", "player", "pitch", "inning"),
    atbat = nums <- c("pitcher", "batter", "num", "b", "s", "o", "inning"),
    pitch = nums <- c(
      "id", "x", "y", "start_speed", "end_speed", "sz_top", "sz_bot", "pfx_x",
      "pfx_z", "px", "pz", "x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az",
      "nasty", "spin_dir", "spin_rate",
      "inning", "num", "on_1b", "on_2b", "on_3b"
    ),
    po = nums <- c("inning", "num"),
    runner = nums <- c("id", "inning", "num")
  )
  # For some reason, records are sometimes duplicated, remove them!
  dat <- unique(dat)
  numz <- nums[nums %in% names(dat)] # error handling (just in case one of the columns doesn't exist)
  for (i in numz) dat[, i] <- suppressWarnings(as.numeric(dat[, i]))
  if (length(grep("^url$", names(dat)))) dat$gameday_link <- sub("/.*", "", sub(".*gid", "gid", dat$url))
  return(dat)
}

# Add columns with relevant pitch count to the 'pitch' table.
appendPitchCount <- function(dat) {
  if (any(!c("type", "gameday_link", "num") %in% colnames(dat))) {
    warning("Count column couldn't be created")
    return(dat)
  }
  balls <- as.numeric(dat[, "type"] %in% "B")
  strikes <- as.numeric(dat[, "type"] %in% "S")
  pre.idx <- paste(dat[, "gameday_link"], dat[, "num"])
  idx <- factor(pre.idx, levels = unique(pre.idx))
  cum.balls <- unlist(tapply(balls, INDEX = idx, function(x) {
    n <- length(x)
    pmin(cumsum(c(0, x[-n])), 3)
  }))
  cum.strikes <- unlist(tapply(strikes, INDEX = idx, function(x) {
    n <- length(x)
    pmin(cumsum(c(0, x[-n])), 2)
  }))
  dat$count <- paste(cum.balls, cum.strikes, sep = "-")
  return(dat)
}

# Add columns with relevant pitch count to the 'pitch' table.
appendDate <- function(dat) {
  if (!("gameday_link" %in% colnames(dat))) {
    warning("'date' column couldn't be created")
    return(dat)
  }
  return(cbind(dat, date = substr(dat[, "gameday_link"], 5, 14)))
}

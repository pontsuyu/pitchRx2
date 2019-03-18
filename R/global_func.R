#' Make Gameday urls
#'
#' @param x
#'
#' @return vector
#'
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

#' Get game_ids in specified years
#'
#' @import rvest
#' @import stringr
#' @importFrom dplyr %>%
#' @importFrom purrr map
#'
#' @return game_id vector
#' @export
#'
get_gids <- function(start_year, end_year){
    if(start_year < 2010 || end_year > lubridate::year(Sys.Date()))
        stop(paste0("'years' must be between 2010 and ", lubridate::year(Sys.Date())))
    `%+%` <- function(x, y) paste0(x, y)
    date_seq <- seq(as.Date(start_year %+% "-02-01"),
                    as.Date(end_year %+% "-10-31"), by = "day")
    date <- data.frame(date = date_seq, stringsAsFactors = F) %>%
        tidyr::separate(date, c("year", "month", "day"), sep = "-")
    candidate <- "http://gd2.mlb.com/components/game/mlb/year_" %+% date$year %+%
        "/month_" %+% date$month %+% "/day_" %+% date$day
    res_list <- candidate %>% map(function(x) {
        tmp <- try(read_html(x), silent = T)
        if (class(tmp)[1] != "try-error") {
            tmp %>%
                html_nodes("li") %>%
                html_text() %>%
                str_subset("^ gid_") %>%
                str_subset(".*mlb.*") %>%
                str_replace_all(" |/", "")
        }
    })
    return(do.call("c", res_list))
}

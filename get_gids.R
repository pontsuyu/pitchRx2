library(rvest)
library(tidyverse)
#' Title
#'
#' @param years 
#'
#' @return
#' @export
#'
#' @examples
get_gids <- function(start_year = 2016, end_year = 2017){
  if(start_year<2010||end_year>lubridate::year(Sys.Date()))
    stop("'years' must be between 2015 and 2017.")

  date <- data.frame(date = seq(as.Date(paste0(start_year, "-01-01")), as.Date(paste0(end_year, "-12-31")), by = "day"), stringsAsFactors = F) %>% 
          separate(date, c("year", "month", "day"), sep = "-")
  candidate <- paste0(
      "http://gd2.mlb.com/components/game/mlb/year_", year = date[["year"]],
      "/month_", month = date[["month"]],
      "/day_", day = date[["day"]]
      )
  res_list <- candidate %>% map(function(x) {
    tmp <- try(read_html(x), silent = T)
    if (all(class(tmp) != "try-error")) {
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

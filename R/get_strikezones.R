#' Calculate strikezone boundaries
#'
#' Strikezone boundaries calculated according to Mike Fast's specifications
#'
#' @param data PITCHf/x orginally entered into \code{animateFX}
#' @references \url{http://www.baseballprospectus.com/article.php?articleid=14572}
#'
#' @import dplyr
#' @importFrom tidyr separate
#'
#' @return Returns a list of boundaries for both right handed batters and left handed batters
#' @export
#'
get_strikezones <- function(data, mean_strikezone = FALSE) {
  if(!all(c("b_height", "stand") %in% colnames(data)))
    stop("'data' must have the following columns: 'b_height', 'stand', 'pz'")
  h <- data %>%
    select(stand, b_height) %>%
    separate(b_height, c("x", "z"), sep = "' ") %>%
    mutate(x = as.numeric(x),
           z = as.numeric(z)/12, # feet to inch
           heights = x + z) %>%
    select(-x, -z)
  rightz <- as.numeric(h$stand == "R")
  leftz <- as.numeric(h$stand == "L")
  h <- h %>%
    mutate(Top = rightz*(2.6 + h$heights*0.136) + leftz*(2 + h$heights*0.229),
           Bottom = rightz*(0.92 + h$heights*0.136) + leftz*(0.35 + h$heights*0.229),
           Left = -1.03*rightz + -1.20*leftz,
           Right = rightz + 0.81*leftz)
  if(mean_strikezone){
    h <- h %>%
      group_by(stand) %>%
      summarise_all(function(x) mean(x, na.rm = T))
  }
  return(as.data.frame(h))
}

#' Animate PITCHf/x
#'
#' Pitch trajectories animated on a two-dimensional plot.
#'
#' \code{animateFX} plots a series of "snapshots" that represent pitch trajectories
#' from the point of release until all of them reach home plate.
#' The graphic takes on the viewpoint of the umpire; that is, the pitches are getting closer
#' to the viewer with time. This is relected with the increase in size of the "balls" as the
#' animation progresses.
#'
#' @param data data frame with appropriately named PITCHf/x variables
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param layers list of ggplot2 layer modifications.
#' @param color character. plot point color.
#' @param point.alpha ggplot2 alpha parameter
#' @param x_lim vector(2 element). x axis range.
#' @param y_lim vector(2 element). y axis range.
#' @param movie.name gif image name
#' @param ... using in saveGIF
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom animation saveGIF
#'
#' @return Returns a series of objects of the class used by package ggplot2 to represent plots.
#'
#' @export
#' @examples
#' \dontrun{
#'   animateFX(pitches, interval = 1.0, ani.height = 600, ani.width = 1200)
#' }
#'

animateFX <- function(data, pitch_interval = 0.01, layers = facet_grid(~stand), color = "pitch_type",
                      point.alpha = 1/3, x_lim = c(-3.5, 3.5), y_lim = c(0, 7),
                      movie.name = "animation.gif", ...) {
    idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
    complete <- data %>% drop_na_(idx)
    snapshots <- get_snapshots(complete, interval = pitch_interval)
    other <- complete %>% select_(.dots = paste0("-", idx))
    boundaries <- get_strikezones(data, mean_strikezone = TRUE)
    N <- dim(snapshots)[2] # Number of plots in animation
    release <- max(as.numeric(complete$y0))
    max.dist <- release - 1.417 # maximum distance a ball can be from the pitcher (1.417 is start of home plate)
    saveGIF(
        for(i in 1:N){
            frame <- data.frame(snapshots[,i,], other, stringsAsFactors = FALSE)
            colnames(frame) <- c("X", "Y", "Z", colnames(other))
            frame$scale_y <- abs(frame$y - release) / max.dist
            p <- ggplot() +
                geom_point(data = frame, aes_string("X", "Z", colour = color), alpha = point.alpha) +
                geom_rect(data = boundaries,
                          aes(ymax = Top, ymin = Bottom, xmax = Right, xmin = Left),
                          alpha = 0, colour = "black") +
                xlim(x_lim) + ylim(y_lim) +
                xlab("Horizontal Pitch Location") + ylab("Height from Ground") +
                theme_light()
            print(p + layers)
        }, movie.name = movie.name, ...)
}

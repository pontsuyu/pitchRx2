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
#' @param point.alpha ggplot2 alpha parameter
#' @param layers list of ggplot2 layer modifications.
#' @param color character. plot point color.
#' @param x_lim vector(2 element). x axis range.
#' @param y_lim vector(2 element). y axis range.
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param ... extra options passed onto geom commands
#'
#' @return Returns a series of objects of the class used by package ggplot2 to represent plots.
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' data(pitches)
#' #generate animation and prompt default web browser to view the sequence of plots
#' \dontrun{
#' animation::saveHTML({ animateFX(pitches, layer = facet_grid(pitcher_name~stand)) })
#' }
#'

animateFX <-
    function(data, point.alpha = 1/3, layers, strike = FALSE, color = "pitch_type_name",
             x_lim = c(-3.5, 3.5), y_lim = c(0, 7), flag = FALSE, interval = 0.01, ...) {
        y_max = y_min = right = left = NULL
        if (!"pitch_type" %in% colnames(data))
            stop("'data' dont have 'pitch_type' column.")
        # Add descriptions as pitch_types
        data <- inner_join(data, pitchRx2::pitch_type, by = "pitch_type") %>% as.data.frame
        if (!"b_height" %in% names(data)) {
            warning("pitchRx2 assumes the height of each batter is recorded as 'b_height'.
                    Since there is no such column, we will assume each batter has a height of 6-2")
            data$b_height <- "6-2"
        }
        idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
        if (!all(idx %in% names(data)))
            stop("'data' must have the following variables in your dataset to animate pitch locations:
                 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")

        for (i in idx) data[, i] <- as.numeric(data[, i])
        complete <- data %>% filter(complete.cases(select(., idx)))
        snapshots <- getSnapshots(complete, interval)
        other <- complete %>% select(-idx)
        boundaries <- getStrikezones(data, strikeFX = strike) #Strikezone boundaries
        other <- inner_join(other, boundaries, by = "stand")
        xrange <- xlim(x_lim)
        yrange <- ylim(y_lim)
        N <- dim(snapshots)[2] # Number of plots in animation
        release <- max(as.numeric(complete$y0))
        max.dist <- release - 1.417 #maximum distance a baseball can be from the pitcher (1.417 is start of home plate)
        swing <- NULL
        aes_mapping <- aes_string(x = "x", y = "z", colour = color)
        if (missing(layers)) layers <- NULL
        for (i in 1:N) {
            frame <- data.frame(snapshots[, i, ], other)
            colnames(frame) <- c("x", "y", "z", colnames(other))
            frame$scale_y <- abs(frame$y - release) / max.dist
            p <- ggplot(data = frame) + xrange + yrange +
                 xlab("Horizontal Pitch Location") + ylab("Height from Ground") +
                 scale_alpha(guide = "none") + scale_size(guide = "none") +
                 theme(legend.position = "right",
                       axis.text.x = element_text(size=15), axis.text.y = element_text(size=15),
                       legend.title = element_text(size=15), legend.text = element_text(size=15),
                       panel.background = element_rect(fill = "white", colour = NA),
                       panel.border = element_rect(fill = NA, colour = "grey20"),
                       panel.grid.major = element_line(colour = "grey92"),
                       panel.grid.minor = element_line(colour = "grey92", size = 0.25),
                       strip.background = element_rect(fill = "grey85", colour = "grey20"),
                       legend.key = element_rect(fill = "white", colour = NA), complete = TRUE)
            p <- p + geom_rect(aes(ymax = Top, ymin = Bottom, xmax = Right, xmin = Left),
                               alpha = 0, colour = "black") +
                geom_point(mapping = aes_mapping, alpha = point.alpha)
            print(p + layers)
        }
    }

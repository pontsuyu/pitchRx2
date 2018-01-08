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
#' @param color variable used to control coloring scheme.
#' @param avg.by variable used as an index for averaging over PITCHf/x parameters
#' @param point.alpha ggplot2 alpha parameter
#' @param limitz limits for horizontal and vertical axes.
#' @param flag indicate whether or not batter has decided to swing.
#' @param interval time (in seconds) between plotting the pitch locations.
#' @param layer list of ggplot2 layer modifications.
#' @param parent is the function being called from a higher-level function? (experimental)
#' @param ... extra options passed onto geom commands
#' @return Returns a series of objects of the class used by package ggplot2 to represent plots.
#' @import ggplot2
#' @export
#' @examples
#' data(pitches)
#' #generate animation and prompt default web browser to view the sequence of plots
#' \dontrun{
#' animation::saveHTML({ animateFX(pitches, layer = facet_grid(pitcher_name~stand)) })
#' animation::saveHTML({ animateFX(pitches, avg.by="pitch_types",
#'                          layer = facet_grid(pitcher_name~stand))
#'                    })
#' }
#'

animateFX <-
    function(data, point.alpha = 1/3, layers, strike = FALSE,
             x_lim = c(-3.5, 3.5), y_lim = c(0, 7), flag = FALSE, interval = 0.01, ...) {
        y_max = y_min = right = left = NULL
        if (!"pitch_type" %in% colnames(data))
            stop("'data' does not have 'pitch_type' column.")
        #Add descriptions as pitch_types
        data <- dplyr::inner_join(data, pitchRx2::pitch_type, by = "pitch_type") %>% as.data.frame
        if (!"b_height" %in% names(data)) {
            warning("pitchRx assumes the height of each batter is recorded as 'b_height'.
                    Since there is no such column, we will assume each batter has a height of 6'2''")
            data$b_height <- "6-2"
        }
        idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
        if (!all(idx %in% names(data)))
            stop("You must have the following variables in your dataset to animate pitch locations:
                 'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
        for (i in idx) data[, i] <- as.numeric(data[, i])
        complete <- data[complete.cases(data[, idx]),]
        parameters <- complete[, idx]
        snapshots <- getSnapshots(parameters, interval)
        other <- complete[,!(colnames(complete) %in% idx)]
        boundaries <- getStrikezones(data, strikeFX = strike) #Strikezone boundaries
        other <- inner_join(other, boundaries, by = "stand")
        xrange <- xlim(x_lim)
        yrange <- ylim(y_lim)
        N <- dim(snapshots)[2] #Number of plots in animation
        release <- max(as.numeric(parameters$y0))
        max.dist <- release - 1.417 #maximum distance a baseball can be from the pitcher (1.417 is start of home plate)
        swing <- NULL
        if(missing(layers)) layers <- NULL
        for (i in 1:(N-1)) {
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
                geom_point(aes(x = "x", y = "z", colour = "pitch_type_name"), alpha = point.alpha)
            print(p + layers)
        }
    }

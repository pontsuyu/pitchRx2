#' Produce time sequenced pitch locations from PITCHf/x parameters
#'
#' @references http://baseball.physics.illinois.edu/KaganPitchfx.pdf
#'
#' @param data The nine PITCHf/x parameters used to determine the location of a pitch at a given time.
#' @param interval the amount of time between 'snapshots'
#' @return Return a three dimensional array. The third dimension corresponds to different 'snapshots' of locations.
#' @export

get_snapshots <- function(data, interval = 0.01) {
  idx <- c("x0", "y0", "z0", "vx0", "vy0", "vz0", "ax", "ay", "az")
  if (!all(idx %in% names(data)))
      stop("'data' must have the following variables in your dataset:
            'x0', 'y0', 'z0', 'vx0', 'vy0', 'vz0', 'ax', 'ay', 'az'")
  parameters <- data[,idx]
  # Figure out how long it takes each pitch to reach home plate
  times <- with(parameters, (-vy0 - sqrt(vy0^2 - 2 * ay * (y0 - 1.417))) / ay)
  # Number of 'snapshots' required for EVERY pitch to reach home plate
  nplots <- ceiling(max(times/interval)) + 1
  # Number of pitches (within each plot)
  npitches <- NROW(data)
  t.matrix <- matrix(rep(0:(nplots - 1)*interval, times = npitches), byrow = TRUE, nrow = npitches)
  # Restrict time if ball already crossed home plate
  t <- pmin(t.matrix, times)
  # Repeat the PITCHf/x parameters for the total amount of plots needed
  snapshots <- array(rep(c(parameters, recursive = TRUE), nplots), dim = c(dim(parameters), nplots))
  # Horizontal location at time t
  x <- snapshots[,1,] + snapshots[,4,] * t + 0.5 * snapshots[,7,] * t^2
  # Distance from batter at time t
  y <- snapshots[,2,] + snapshots[,5,] * t + 0.5 * snapshots[,8,] * t^2
  # Height from ground at time t
  z <- snapshots[,3,] + snapshots[,6,] * t + 0.5 * snapshots[,9,] * t^2
  return(array(c(x, y, z), dim = c(npitches, nplots, 3)))
}

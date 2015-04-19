# Use this function to subset out a particular portion of an experiment (to clip
# off unused data from beginnning and end).

subsetTime <- function(DAMdfr, startTime = 0, expDuration = dim(DAMdfr)[1],
                       units = c("seconds", "minutes", "hours")) {
  # Parse them arguments...
  units <- match.arg(units)
  startTime <- toSeconds(startTime, units)
  expDuration <- toSeconds(expDuration, units)
  interval <- getInterval(DAMdfr)

  # Fix bad values to aid indexing.
  if (!is.integer(startTime / interval)) {
    warning("startTime is not a multiple of data interval, coercing to integer.")
    startTime <- floor(startTime / interval) * interval
  }
  if (!is.integer(expDuration / interval)) {
    warning("expDuration is not a multiple of data interval, coercing to integer.")
    expDuration <- floor(expDuration / interval) * interval
  }

  # Okay subset out and return the data we want.
  return(DAMdfr[hours_start:(hours_start + exp_duration), ])
}

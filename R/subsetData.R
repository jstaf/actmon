# Use this function to subset out a particular portion of an experiment (to clip
# off unused data from beginnning and end).

subsetData <- function(DAMdfr, startTime = 0, expDuration = dim(DAMdfr)[1],
                       units = c("seconds", "minutes", "hours")) {
  units <- match.arg(units)
  startTime <- toSeconds(startTime, units)
  expDuration <- toSeconds(expDuration, units)
  interval <- getInterval(DAMdfr)

  if (!is.integer(startTime / interval)) {
    warning("startTime is not a multiple of data interval, coercing to integer.")
  }

  if (!is.integer(expDuration / interval)) {
    warning("expDuration is not a multiple of data interval, coercing to integer.")
  }

  # Okay subset out and return the data we want.
  return(DAMdfr[hours_start:(hours_start + exp_duration), ])
}

# Converts "units" args in other functions to seconds.

toSeconds <- function(time, units = c("seconds", "minutes", "hours")) {
  units <- match.arg(units)

  # Convert target to seconds
  if (units == "minutes") {
    return(time * 60)
  } else if (units == "hours") {
    return(time * 3600)
  } else {
    return(time)
  }
}

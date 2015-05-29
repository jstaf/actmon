#' Convert a time to seconds
#' 
#' A utility function used to converts "units" args in other functions to
#' seconds.
#' 
#' @param time
#' @param units
#'   
#' @return time in seconds
#' @export
#' 
toSeconds <- function(time, units = c("seconds", "minutes", "hours", "days")) {
  units <- match.arg(units)

  # Convert target to seconds
  if (units == "minutes") {
    return(time * 60)
  } else if (units == "hours") {
    return(time * 3600)
  } else if (units == "days") {
    return(time * 86400)
  } else {
    return(time)
  }
}

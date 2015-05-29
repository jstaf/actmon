#' Calculate sleep
#' 
#' Calculates the amount of sleep in a dataset. This function is recommended to 
#' be run on datasets with 5 minute intervals between measurements. Data at
#' "faster" rates will be coerced to 5 minutes.
#' 
#' @param obj A valid DAM object (created by \code{\link{newExperiment}})
#'   
#' @return A DAM S4 object.
#' @export
#' 
#' @examples 
#' calcSleep(DAM_DD)
setGeneric("calcSleep", function(obj) {standardGeneric("calcSleep")})
setMethod("calcSleep", signature = "DAM",
          definition = function(obj) {
            rate <- getInterval(obj)
            if (rate < 300) {
              warning("Data rate is less than 5 min/reading, aggregating by sum.")
              obj <- toInterval(obj, 5, units = "minutes", aggregateBy = "sum")
            } else if (rate > 300) {
              warning("Data rate is greater than 5 minutes per reading, sleep may be underestimated.")
            }
            
            vals <- getVals(obj@data)
            vals <- apply(vals, c(1, 2), isAsleep)
            obj@data <- setVals(obj@data, vals)
            
            return(obj)
          })

#' Calculate sleep values
#' 
#' Changes raw counts to sleep yes/no over that interval. Do not use time
#' intervals under 5 minutes.
#' 
#' @param count A raw number of counts
#'   
#' @return Whether or not a fly was asleep during that interval.
#' 
#' @examples
#' isAsleep(5)
#' isAsleep(NA)
isAsleep <- function(count) {
  if (is.na(count)) {
    return(NA)
  } else if (count > 0) {
    return(0)
  } else {
    return(100)
  }
}

#' Convert sleep percentages to minutes
#' 
#' This method converts a dataset with sleep percentages to a dataset with sleep
#' measured in minutes. DONT RUN THIS ON DATA WHERE YOU HAVE NOT CALCULATED
#' SLEEP.
#' 
#' @param obj A valid DAM S4 object (\code{\link{calcSleep}} must have already 
#'   been performed)
#'   
#' @return A DAM dataset with sleep values in minutes.
#' @export
#' 
#' @examples
#' sleep <- calcSleep(DAM_DD)
#' sleep <- percentToMins(sleep)
setGeneric("percentToMins", function(obj) {standardGeneric("percentToMins")})
setMethod("percentToMins", signature = "DAM", 
          definition = function(obj) {
            interval <- getInterval(obj)
            vals <- getVals(obj@data)
            # convert sleep percentages to minutes
            vals <- (vals / 100) * interval / 60
            
            obj@data <- setVals(obj@data, vals)
            return(obj)
          })

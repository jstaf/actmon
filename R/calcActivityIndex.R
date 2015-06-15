#' Calculate activity index
#' 
#' The activity index of a fly is the amount of activity counts it had during a
#' day / the total time it was awake.
#' 
#' @param obj A valid DAM S4 object
#'   
#' @return Returns a vector of the activity indexes for each fly
#' @export
#' 
#' @examples
#' calcActivityIndex(dropDead(DAM_DD))
setGeneric("calcActivityIndex", function(obj) {standardGeneric("calcActivityIndex")})
setMethod("calcActivityIndex", signature = "DAM", 
          definition = function(obj) {
            vals <- getVals(obj@data)
            indexes <- apply(vals, 2, activityIndex, getInterval(obj))
            
            return(indexes)
          })

#internal function used to calculate activity index
# activity index is the number of counts/day divided by the # of minutes awake/day
activityIndex <- function(vector, interval) {
  # total # of indices / portion of 1 day that an index represents
  # days <- length(vector) / (interval / 86400)
  
  # remove points where the fly was alseep
  vector[vector == 0] <- NA
  vector <- vector[!is.na(vector)]
  
  #convert interval to minutes and compute activity index
  interval <- interval / 60
  timeAwake <- (length(vector) * interval)
  actIndex <- sum(vector, na.rm = TRUE) / timeAwake
  
  return(actIndex)
}

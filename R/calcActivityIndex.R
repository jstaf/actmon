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
            indexes <- apply(vals, 2, activityIndex)
            
            return(indexes)
          })

#internal function used to calculate activity index
activityIndex <- function(vector, interval) {
  # remove points where the fly was alseep
  vector[vector == 0] <- NA
  
  # NEED VALUES FOR INTERVAL FROM AARYA'S PAPER
  interval <- interval / 3600
  timeAwake <- (length(vector[!is.na(vector)]) * interval)
  actIndex <- sum(vector, na.rm = TRUE) / timeAwake
  
  return(actIndex)
}

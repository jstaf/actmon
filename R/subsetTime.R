#' Subsetting data by time
#' 
#' This function subsets your data to correspond to a certain time interval. Use
#' this function to subset out a particular portion of an experiment (to clip 
#' off unused data from beginnning and end).
#' 
#' @param obj A valid DAM object (created by \code{\link{newExperiment}}).
#' @param startTime The starting point to include.
#' @param expDuration The duration of time you wish to include.
#' @param units The units of time you are using. Can be one of the following:
#'   "days", "hours", "minutes", or "seconds."
#'   
#' @return Returns a DAM S4 object.
#'   
#' @export
#' 
#' @examples
#' returnedDAM <- subset(DAM, startTime = 1, expDuration = 3, units = "days")
#' 
setGeneric("subsetTime", function(obj, startTime, expDuration, units) {
  standardGeneric("subsetTime")
})
setMethod("subsetTime", signature = "DAM",
          definition = function(obj, startTime = 0, expDuration = dim(obj)[1],
                                units = c("seconds", "minutes", "hours", "days")) {
            # Parse them arguments...
            units <- match.arg(units)
            startTime <- toSeconds(startTime, units)
            expDuration <- toSeconds(expDuration, units)
            interval <- getInterval(obj)
            
            # Fix bad values to aid indexing.
            if (startTime %% interval != 0) {
              warning("startTime is not a multiple of data interval, coercing to integer.")
              startTime <- floor(startTime / interval) * interval
            }
            if (expDuration %% interval != 0) {
              warning("expDuration is not a multiple of data interval, coercing to integer.")
              expDuration <- floor(expDuration / interval) * interval
            }
            
            # Okay subset out and return the data we want. 
            toReturn <- (startTime / interval):((startTime / interval) + (expDuration / interval) - 1)
            obj@data <- obj@data[toReturn, ]
            
            # Clean up read indices
            obj@data$read_index[!is.na(obj@data$read_index)] <- 
              1:length(obj@data$read_index[!is.na(obj@data$read_index)])
            
            return(obj)
          })

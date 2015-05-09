# Use this function to subset out a particular portion of an experiment (to clip
# off unused data from beginnning and end).
setGeneric("subsetTime", function(obj, startTime, expDuration, units) {
  standardGeneric("subsetTime")
})
setMethod("subsetTime", signature = "DAM",
          definition = function(obj, startTime = 0, expDuration = dim(obj)[1],
                                units = c("seconds", "minutes", "hours")) {
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
            toReturn <- (startTime / interval):((startTime / interval) + (expDuration / interval))
            obj@data <- obj@data[toReturn, ]
            return(obj)
          })

# Applies the function isAsleep to the dataset.
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

# Changes raw counts to sleep yes/no per 5 min interval.
isAsleep <- function(count) {
  if (is.na(count)) {
    return(NA)
  } else if (count > 0) {
    return(0)
  } else {
    return(100)
  }
}

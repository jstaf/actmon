# use this ONLY when toInterval() has not been run
# use this ONLY on a single day's worth of data
setGeneric("calcDayTotals", function(obj, isSleepData, ..., separateLightDark = FALSE) {
  standardGeneric("calcDayTotals")})
setMethod("calcDayTotals", signature = "DAM", 
           definition = function(obj, isSleepData, separateLightDark) {
             interval <- getInterval(obj)
             # check length of data, throw error if longer than 1 day
             if (86400 > (3600 / interval * length(obj@data[, 1]))) {
               warning("Data is longer than 1 day. Coercing data to a 24 hour average.")
               obj <- toAvgDay(obj, incomplete.rm = FALSE)
             }
             
             vals <- getVals(obj@data)
             if (isSleepData) {
               # convert sleep percentages to minutes
               (vals / 100) * interval / 60
             }
             obj <- setVals(obj@data, vals)
             
             # okay now aggregate all of each column by sum
             if (separateLightDark) {
               if (length(obj@data[, 1]) %% 2 != 0) {
                 stop("Data has an uneven number of rows and cannot be split.")
               }
               obj <- toInterval(obj, 12, units = "hours", aggregateBy = "sum")
             } else {
               obj <- toInterval(obj, 24, units = "hours", aggregateBy = "sum")
             }
             
             return(obj)
           })


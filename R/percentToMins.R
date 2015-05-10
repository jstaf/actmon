# convert sleep percentages to minutes
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

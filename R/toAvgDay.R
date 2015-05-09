# flattens days to an average day... data from each individual fly is calculated separately
setGeneric("toAvgDay", function(DAM, ..., incomplete.rm = TRUE) {standardGeneric("toAvgDay")})
setMethod("toAvgDay", signature = "DAM",
          definition = function(DAM, ..., incomplete.rm) {
            dayLength <- 3600 / getInterval(DAM) * 24
            
            if (incomplete.rm) {
              days <- (length(DAM@data$read_index) %/% dayLength)
            } else {
              days <- ceiling(length(DAM@data$read_index) / dayLength)
            }
            
            avgDay <- newTemplate(DAM, dayLength)
            
            # these are the values to calculate means of
            corrIdx <- (1:days * dayLength) - dayLength
            
            vals <- getVals(DAM@data)
            for (i in 1:dayLength) {
              vals[i, ] <- apply(vals[corrIdx + i, ], 2, mean, na.rm = TRUE)
            }
            # take only the averaged first day
            avgDay@data <- setVals(avgDay@data, vals[1:dayLength, ])
            return(avgDay)
          })

# finds the lighting interval in hours
setGeneric("findLightInterval", function(DAM) {standardGeneric("findLightInterval")})
setMethod("findLightInterval", signature = "DAM",
          definition = function(DAM) {
            diff <- getLightChanges(DAM)
            
            # remove the first and last values for a cleaner average
            diff <- diff[2:(length(diff) - 1)]
            # take the geometric mean and find which side of the range it's closest to
            avg <- (prod(diff)) ^ (1 / length(diff))
            rng <- range(diff)
            ind <- which.min(abs(rng - avg))
            
            hours <- rep(NA, 10000)
            seed <- 100
            for (i in round(avg):rng[ind]) {
              hours[i] <- difftime(DAM@data$read_time[seed + i],
                                   DAM@data$read_time[seed],
                                   units = "hours")
            }
            hours <- hours[is.na(hours) == FALSE]
            # retrieve even numbers of hours
            ans <- hours[round(hours) == hours]
            return(ans)
          })

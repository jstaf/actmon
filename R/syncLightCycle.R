# chop experiment down to complete days
setGeneric("syncLightCycle", function(DAM, ..., completeFirstDay = FALSE, lightFirst = TRUE) {
  standardGeneric("syncLightCycle")
})
setMethod("syncLightCycle", signature = "DAM",
          definition = function(DAM, ..., completeFirstDay, lightFirst) {
            # First index is where we start cutting things
            tryCatch(first <- getLightChanges(DAM)[1],
                     error = function(e) {
                       stop(paste(e, "Not enough light changes detected."))
                     })
            idxPerHour <- 3600 / getInterval(DAM)
            
            # lag our start point by 12 hours if the first light status != lightFist
            if (DAM@data$light_status[first - 1] != lightFirst) {
              first <- first + idxPerHour * 12 
            }
            
            # add NANs to first day to get it in sync if first day does not need
            # to be complete
            if (!completeFirstDay) {
              # how many NANs do we need to make
              toCreate <- (idxPerHour * 12) - first
              if (toCreate > 0) {
                dummy <- as.data.frame(matrix(nrow = toCreate, ncol = dim(DAM@data)[2]))
                colnames(dummy) <- colnames(DAM@data)
                # need to format dummy's dates as posixctlength(DAM@data$read_index) %/% dayLength
                dummy$read_time <- as.POSIXct(dummy$read_time)
                
                DAM@data <- rbind(dummy, DAM@data)
              }
              # we're going to take the whole dataset start now
              first <- 0
            }
            
            hours <- (length(DAM@data$read_index) - first) / idxPerHour
            days <- (hours %/% 24) * 24
            
            return(subsetTime(DAM, 
                              startTime = first, 
                              expDuration = days, 
                              units = "hours"))
          })

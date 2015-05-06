
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

# chop experiment down to complete days
setGeneric("syncLightCycle", function(DAM, completeFirstDay, lightFirst) {
  standardGeneric("syncLightCycle")
  })
setMethod("syncLightCycle", signature = "DAM",
          definition = function(DAM, completeFirstDay = TRUE, lightFirst = TRUE) {
            # First index is where we start cutting things
            first <- getLightChanges(DAM)[1]
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
              dummy <- as.data.frame(matrix(nrow = toCreate, ncol = dim(DAM@data)[2]))
              colnames(dummy) <- colnames(DAM@data)
              # need to format dummy's dates as posixct
              dummy$read_time <- as.POSIXct(dummy$read_time)
              
              DAM@data <- rbind(dummy, DAM@data)
              
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

setGeneric("getLightChanges", function(DAM) {standardGeneric("getLightChanges")})
setMethod("getLightChanges", signature = "DAM",
          definition = function(DAM) {
            changed <- whichChanged(DAM@data$light_status, 0)
            
            # find difference in indices
            diff <- rep(NA, length(changed) - 1)
            diff[1] <- changed[1]
            for (i in 2:length(diff)) {
              diff[i] <- changed[i + 1] - changed[i]
            }
            
            # Which indices are dramatically shorter than others (probably day/light 
            # cycles that were broken up...)? 
            idx <- which(diff < (mean(diff) / 2))
            streaks <- c(1, whichChanged(idx, 1), length(idx))
            
            # assemble more parsimonious indexes
            for (i in 2:length(streaks)) {
              # Combine streaks of smaller values
              if (i != length(streaks)) {
                idxInStreak <- streaks[i - 1]:(streaks[i] - 1)
              } else {
                # need to grab the last index here
                idxInStreak <- streaks[i - 1]:streaks[i]
              }
              streakSum <- sum(idx[idxInStreak])
              
              # now find which "side" to add the streak sum to
              left <- idx[idxInStreak[1]] - 1
              right <- idx[idxInStreak[length(idxInStreak)]] + 1
              if (left < right) {
                diff[left] <- diff[left] + streakSum
              } else {
                diff[right] <- diff[right] + streakSum
              }
            }
            # remove the small value streaks from diff
            return(diff[-idx])
          })

# find indices of which values which differed by more than a certain delta
whichChanged <- function(vector, delta) {
  changed <- rep(FALSE, length(vector))
  for (i in 2:length(vector)) {
    if (abs(vector[i] - vector[i - 1]) > delta) {
      changed[i] <- TRUE
    }
  }
  return(which(changed))
}

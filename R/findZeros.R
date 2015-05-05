
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

setGeneric("syncLightCycle", function(DAM, hourPerCycle, completeCycle, lightFirst) {
  standardGeneric("syncLightCycle")
  })
setMethod("syncLightCycle", signature = "DAM",
          definition = function(DAM, hourPerCycle,
                               completeCycle = TRUE, lightFirst = TRUE) {
            diff <- getLightChanges(DAM)
            
            return(subsetTime(DAM, start, end, "hours") )
          })

setGeneric("getLightChanges", function(DAM) {standardGeneric("getLightChanges")})
setMethod("getLightChanges", signature = "DAM",
          definition = function(DAM) {
            changed <- whichChanged(DAM@data$light_status, 0)
            
            # find difference in indices
            diff <- rep(NA, length(changed) - 1)
            for (i in 1:length(diff)) {
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

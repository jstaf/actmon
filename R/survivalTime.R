# applies the survival time function across columns
setGeneric("calcSurvivalTime", function(obj, ..., endThresh = 4) {standardGeneric("calcSurvivalTime")})
setMethod("calcSurvivalTime", signature = "DAM",
          definition = function(obj, ..., endThresh) {
            vals <- getVals(obj@data)
            survival <- apply(vals, 2, survivalTime, threshold = 5)
            
            # flies need to have been motionless for several hours before they
            # can be considered "dead" ...
            idxPerHour <- 3600 / getInterval(obj)
            hours <- (length(obj@data[, 1]) - survival) / idxPerHour
            
            # whichever were motionless for less than endThresh hours at the end
            # of the recording are NOT dead
            survival[which(hours < endThresh)] <- NA
            
            return(survival)
          })

# Computes the index at which a fly died in the data
survivalTime <- function(vector, threshold) {
  zeroCounts <- which(vector < threshold)
  # zeroCounts must not be empty or have a length of 1
  if (length(zeroCounts) > 1) {
    # find streaks of zeros
    streaks <- whichChanged(zeroCounts, 1)
    
    # all following values after the last streak must be less than the movement threshold
    isAlive <- any(vector[zeroCounts[streaks[length(streaks)]]]:vector[length(vector)] > threshold)
    if (isAlive) {
      ans <- NA
    } else {
      # last index of streaks marks death
      ans <- zeroCounts[streaks[length(streaks)]]
    }
  } else {
    # no periods of no movement found
    # NOTE: does not accept no movement during only the last index as death
    ans <- NA
  }
  return(ans)
}

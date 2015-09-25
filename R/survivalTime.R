# applies the survival time function across columns

#' Calculate time of death
#' 
#' This method determines at which point an animal died during an experiment. 
#' This is computed by finding the last time period in which an animal moved. If
#' an animal continued moving (and was alive) until the end of an experiment, 
#' this function returns NA for that animal.
#' 
#' @param obj A valid DAM S4 object.
#' @param ...
#' @param endThresh An optional parameter specifying how long (in hours) an
#'   animal must remain motionless before it can be considered "dead." Does not
#'   affect the index of death.
#'   
#' @return Returns the index at which animals in a dataset died. Returns NAs for
#'   animals that remained alive at experiment conclusion.
#' @export
#' 
#' @examples
#' calcSurvivalTime(DAM_DD)
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

#' Computes the index at which a fly died in the data
#' 
#' The algorithm that computes time-of-death for flies in a DAM experiment.
#' 
#' @param vector A column of the dataset to operate on.
#' @param threshold The threshold defines a base number of counts, below which
#'   could be considered spurious counts. Designed to prevent a random count
#'   here and there from affecting the time-of-death measurement.
#'   
#' @return Time-of-death (NA if fly was alive)
#' 
survivalTime <- function(vector, threshold) {
  # make NA safe for use after syncLightCycle()
  vector <- vector[!is.na(vector)]
  zeroCounts <- which(vector < threshold)
  
  if (length(zeroCounts) == length(vector) || 
      length(zeroCounts) == length(vector) - 1) {
    # the fly was dead the whole time
    ans <- 1
    # zeroCounts must not be empty or have a length of 1
  } else if ((length(zeroCounts) > 1) &&
             # just being safe
             (length(zeroCounts) != length(vector))) { 
    # find streaks of zeros
    streaks <- whichChanged(zeroCounts, 1)
    
    # all following values after the last streak must be less than the movement threshold
    isAlive <- any(vector[zeroCounts[streaks[length(streaks)]]]:
                     vector[length(vector)] > threshold)
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

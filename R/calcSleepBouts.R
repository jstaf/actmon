#' Calculate mean number of sleep bouts per day
#' 
#' This function calculates the mean number of sleep bouts per day for each fly.
#' 
#' @param obj A valid DAM S4 object where \code{\link{calcSleep}} has already
#'   been performed.
#'   
#' @return A named vector of the number of sleep bouts/length of your experiment in days
#' @export
#' 
#' @examples 
#' sleep <- calcSleep(dropDead(DAM_DD))
#' calcNumBouts(sleep)
setGeneric("calcNumBouts", function(obj) {standardGeneric("calcNumBouts")})
setMethod("calcNumBouts", signature = "DAM",
          definition = function(obj) {
            bouts <- getSleepBouts(obj)
            boutNum <- unlist(lapply(bouts, length))
            
            readTimes <- obj@data$read_time
            readTimes <- readTimes[!is.na(readTimes)]
            days <- as.numeric(difftime(readTimes[length(readTimes)],
                                        readTimes[1],
                                        units = "days"))
            return(boutNum / days)
          })

#' Calculate mean duration of each sleep bout
#' 
#' This method calculates the mean duration of each sleep bout in minutes.
#'
#' @inheritParams calcNumBouts
#'
#' @return A named vector with the duration of each bout in minutes.
#' @export
#'
#' @examples 
#' sleep <- calcSleep(dropDead(DAM_DD))
#' calcMeanBout(sleep)
setGeneric("calcMeanBout", function(obj) {standardGeneric("calcMeanBout")})
setMethod("calcMeanBout", signature = "DAM",
          definition = function(obj) {
            bouts <- getSleepBouts(obj)
            bouts <- unlist(lapply(bouts, mean))
            bouts[is.na(bouts)] <- 0 # for flies that never sleep
            return(bouts)
          })

#' Calculate duration and number of sleep bouts
#' 
#' Returns a list with the duration of each sleep bout for each fly.
#' 
#' @param obj A valid DAM S4 object (created by \code{\link{newExperiment}})
#'   
#' @return Returns a list with the duration of each sleep bout (in minutes).
#' @export
#' 
setGeneric("getSleepBouts", function(obj) {standardGeneric("getSleepBouts")})
setMethod("getSleepBouts", signature = "DAM",
          definition = function(obj) {
            vals <- getVals(obj@data)
            
            allBouts <- apply(vals, 2, calcBouts)
            # convert to minutes
            allBouts <- lapply(allBouts, "*", getInterval(obj) / 60)
            
            return(allBouts)
          })

#' Calculate bout number and duration for a single animal
#' 
#' @param vector This is a 1D vector pertaining to a single fly's sleep data.
#'   
#' @return Returns sleep bout and duration in terms of number of indices where
#'   the fly was asleep.
#'   
calcBouts <- function(vector) {
  vector <- vector[!is.na(vector)] # make this function NA-proof
  
  logic <- logical(length(vector))
  logic[vector != 0] <- TRUE # TRUE if asleep
  bouts <- whichChanged(logic, 0)
  
  # catch flies that NEVER sleep
  if (length(bouts) == 0) {
    return(integer(0))
  }
  
  #okay so we have the start and end of the bouts, now which corresponds to the
  #start, and which corresponds to the end
  
  # if started as asleep, even indices correspond to bout starts
  if (logic[1]) { 
    starts <- c(0, bouts[seq(2,length(bouts), 2)]) # include 0
    ends <- bouts[seq(1, length(bouts), 2)]
  } else {
    starts <- bouts[seq(1, length(bouts), 2)]
    ends <- c(bouts[seq(2,length(bouts),2)])
  }
  
  if (length(starts) > length(ends)) {
    ends[length(starts)] <- bouts[length(bouts)]
  }
  finalBouts <- ends - starts
  # remove zero-length bouts (could just be my test case here)
  finalBouts <- finalBouts[finalBouts != 0] 
  return(finalBouts)
}

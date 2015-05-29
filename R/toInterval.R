#' Collapse a dataset to a target time interval
#' 
#' Use this function to change the time between measurements of a DAM dataset 
#' (can only be used to increase the interval between measurements). When 
#' compressing data to the new set of timepoints, data is aggreagated by either
#' mean or sum.
#' 
#' @param obj A valid DAM experiment object
#' @param target What time interval should the resulting dataset be?
#' @param units The units used by the "target" parameter. Can be one of the 
#'   following: "seconds", "minutes", "hours", "days"
#' @param aggregateBy When aggregating multiple timepoints' worth of data, 
#'   should data be averaged or summed? Can be one of the following: "sum", 
#'   "average"
#'   
#' @return Returns a DAM S4 object, with measurments at the specified time 
#'   interval.
#' @export
#' 
#' @examples
#' getInterval(DAM_DD)
#' returnedDAM <- toInterval(DAM_DD, 1, units = "hours", aggregateBy = "sum")
#' getInterval(returnedDAM)
setGeneric("toInterval", function(obj, target, units, aggregateBy) {standardGeneric("toInterval")})
setMethod("toInterval", signature = "DAM",
          definition = function(obj, target,
                                units = c("seconds", "minutes", "hours", "days"),
                                aggregateBy = c("sum", "average")) {
            #Parse args
            units <- match.arg(units)
            target <- toSeconds(target, units)
            aggregateBy <- match.arg(aggregateBy)
            
            # Check that we are not artificially increasing data resolution.
            
            interval <- getInterval(obj)
            if (interval > target) {
              stop("End interval cannot be smaller than start interval.")
            }
            
            # Okay now scale the data
            DAMobject <- obj@data
            scale <- target / interval
            countsMatrix <- getVals(DAMobject)
            numInt <- length(countsMatrix[,1]) %/% scale
            remainder <- length(countsMatrix[,1]) %% scale
            if (aggregateBy == "sum") {
              compressed <- colSums(matrix(
                countsMatrix[1:(length(countsMatrix[,1]) - remainder), ], nrow = scale))
            } else {
              compressed <- colMeans(matrix(
                countsMatrix[1:(length(countsMatrix[,1]) - remainder), ], nrow = scale))
            }
            compressed <- matrix(compressed, nrow = numInt, ncol = ncol(countsMatrix))
            
            # Slice out the proper data labels and recombine.
            newDAM <- DAMobject[(1:length(compressed[, 1])) * scale, ] 
            newDAM <- setVals(newDAM, compressed)
            
            # Calculate proper light status data
            newLights <- colMeans(matrix(
              DAMobject$light_status[1:(length(DAMobject[,1]) - remainder)], nrow = scale))
            newDAM$light_status <- as.integer(round(matrix(newLights, nrow = numInt, ncol = 1)))
            
            # Cleanup rownames/indices for future operations.
            newDAM$read_index <- 1:length(newDAM$read_index)
            rownames(newDAM) <- newDAM$read_index
            
            # Replace old object data
            obj@data <- newDAM
            
            return(obj)
          })

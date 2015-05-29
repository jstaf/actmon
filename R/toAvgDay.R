#' Compute average day
#' 
#' Averages the data from multiple days to create an averaged "typical day."
#' Data from each individual animal is handled separately.
#' 
#' @param DAM A valid DAM S4 experiment
#' @param ...
#' @param incomplete.rm A boolean value specifying whether incomplete days at 
#'   the end of the dataset should not be included in the calculation. Default: 
#'   TRUE
#'   
#' @return Returns a DAM S4 experiment containing one day's worth of averaged 
#'   measurements.
#' @export
#' 
#' @examples
#' toAvgDay(DAM_DD)
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

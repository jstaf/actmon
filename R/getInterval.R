#' Determine measurement interval
#' 
#' This method calculates the time difference between measurements in your
#' dataset.
#' 
#' @param obj A valid DAM S4 object.
#'   
#' @return Time interval between measurements in seconds.
#' @export
#' 
#' @examples
#' getInterval(DAM_DD)
setGeneric("getInterval", function(obj) {standardGeneric("getInterval")})
setMethod("getInterval", signature = "DAM",
          definition = function(obj) {
            # remove nas
            temp <- na.omit(obj@data)
            idxDiff <- temp[2, 1] - temp[1, 1]
            if (is.na(idxDiff) || (idxDiff < 1)) {
              stop("Something is wrong with the first few read indices of your data.")
            }
            
            return(as.numeric(difftime(temp[2, 2], temp[1, 2], units = "secs")) / idxDiff)
          })

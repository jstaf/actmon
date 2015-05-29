#' Remove an animal from the experiment
#' 
#' This function removes a specified animal from a dataset. Can operate on 
#' vectors, removing multiple animals in a single operation. To remove animals 
#' by their metadata, such as "remove all females", use
#' \code{\link{dropAttribute}} instead.
#' 
#' @seealso \code{\link{dropAttribute}}
#'   
#' @param obj A valid DAM S4 object
#' @param vialNumbers Which vial numbers to remove from a dataset. Can be a 
#'   vector.
#'   
#' @return A DAM S4 object.
#' @export
#' 
#' @examples
#' returnedDAM <- dropAnimals(DAM_DD, 12)
#' 
#' returnedDAM <- dropAnimals(DAM_DD, c(1:3))
setGeneric("dropAnimals", function(obj, vialNumbers) {standardGeneric("dropAnimals")})
setMethod("dropAnimals", signature = "DAM", 
          definition = function(obj, vialNumbers) {
            vialNumbers <- as.character(vialNumbers)
            
            # remove vials from sample info
            idx <- which(obj@sample_info[, 1] %in% vialNumbers)
            obj@sample_info <- obj@sample_info[-idx, ]
            
            # remove vials from datasheet
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, -offsetIdx]
            
            return(obj)
          })

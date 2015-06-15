#' Concatenate experiments
#' 
#' Use this function to mash experiments together. There is no restriction to 
#' doing this by timestamp, etc. - so be careful what you combine. The vial 
#' indices in the second experiment are relabeled to start where the first 
#' experiment stops. All metadata is maintained throughout the operation. Longer
#' experiments are coerced down to the size of the smallest experiment.
#' 
#' @param obj_vector A vector of datasets you wish to concatenate. Time 
#'   intervals for each dataset must be the same.
#'   
#' @return A DAM S4 object.
#' @export
#' 
#' @examples 
#' biggerDAM <- catExperiments(c(DAM_DD, DAM_DD))
setGeneric("catExperiments", function(obj_vector) {standardGeneric("catExperiments")})
setMethod(f = "catExperiments", signature = "list",
          definition = function(obj_vector) {
            if (length(obj_vector) == 1) stop("catExperiments() requires more than one object.")
            
            for (i in (1:length(obj_vector)) ) {
              if (class(obj_vector[[i]]) != "DAM") stop("One or more of the objects you are concatenating is not a valid DAM object.")
            }
            
            obj1 <- obj_vector[[1]]
            for (j in (2:length(obj_vector)) ) {
              obj2 <- obj_vector[[j]]
              
              # check that both experiments are of the same data rate
              if (getInterval(obj1) != getInterval(obj2)) {
                stop("Data is of unequal rates. Run toInterval() on the higher
                     resolution experiment before concatenating.")
              }
              
              # retrieve last vial# in obj1
              last <- as.numeric(colnames(obj1@data)[length(colnames(obj1@data))])
              
              # rename all vial#s in obj2
              numVials <- length(obj2@sample_info[, 1])
              obj2@sample_info[, 1] <- (last + 1):(last + numVials)
              
              lightsCol <- which(colnames(obj2@data) == "light_status")
              colnames(obj2@data)[lightsCol + 1:numVials] <- (last + 1):(last + numVials)
              
              # combine objects
              tryCatch(obj1@sample_info <- rbind(obj1@sample_info, obj2@sample_info),
                       error = function(e) {
                         print(e)
                         stop("Sample info column names do not match. Aborting merge.")
                       })
              
              # cut data down to smaller experiments' size THEN combine
              long1 <- length(obj1@data[, 1])
              long2 <- length(obj2@data[, 1])
              if (long1 != long2) {
                warning("Data is of unequal length. Coercing to equal size
                      (data at the end of the longer experiment will be lost).")
                if (long1 > long2) {
                  obj1@data <- obj1@data[1:long2, ]
                } else {
                  obj2@data <- obj2@data[1:long1, ]
                }
              }
              obj1@data <- cbind(obj1@data, getVals(obj2@data))
              }
            
            return(obj1)
          })

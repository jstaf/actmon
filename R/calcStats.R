#' Calculate statistics
#' 
#' Iterate through possible variations of an attribute in your dataset, 
#' calculating averages/standard error of the mean/stats for each. Returns a 
#' DAMstats object.
#' 
#' A 1-way ANOVA is performed across an attribute on data with a single 
#' timepoint. A 2-way ANOVA is performed across an attribute on data with 
#' multiple timepoints (for performance reasons, this is not done on data with
#' an extreme number of timepoints).
#' 
#' @param obj A valid DAM object (created by \code{\link{newExperiment}}
#' @param attribute Which attribute stats should be calculated against.
#'   
#' @return A DAMstats S4 Object
#' @export
#' 
#' @examples
#' activity <- toInterval(DAM_DD, 1, units = "hours", aggregateBy = "sum")
#' statsAct <- calcStats(activity, "genotype")
setGeneric("calcStats", function(obj, attribute) {standardGeneric("calcStats")})
setMethod("calcStats", signature = "DAM",
          definition = function(obj, attribute){
            # retrieve all values
            stat <- newStats(obj, attribute)
            variable <- listAttribVals(obj, attribute)
            
            # iterate through values and calc avg/sem
            i <- 1
            for (var in variable) {
              temp <- getVals(byAttribute(obj, var, attribute)@data)
              stat@averages[, i] <- rowMeans(temp)
              stat@SEM[, i] <- apply(as.matrix(temp), 1, stdError)
              i <- i + 1
            }
            colnames(stat@averages) <- variable
            colnames(stat@SEM) <- colnames(stat@averages)
            
            # okay do a one-way anova if there's only one row
            if (length(obj@data[, 1]) == 1) {
              calcAnova1(obj, 1, attribute)
            } else {
              calcAnova2(obj, attribute)
            }
            
            return(stat)
          })

          

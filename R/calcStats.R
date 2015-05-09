# Iterate through possible variations of an attribute in your dataset,
# calculating averages/standard error of the mean/stats for each. Returns a
# DAMstats object.
setGeneric("calcStats", function(obj, attribute) {standardGeneric("calcStats")})
setMethod("calcStats", signature = "DAM",
          definition = function(obj, attribute){
            # retrieve all values
            stat <- newStats(obj, attribute)
            variable <- listAttribVals(obj, attribute)
            
            # iterate through values and calc stats
            i <- 1
            for (var in variable) {
              temp <- getVals(byAttribute(obj, var, attribute)@data)
              stat@averages[, i] <- rowMeans(temp)
              stat@SEM[, i] <- apply(as.matrix(temp), 1, stdError)
              i <- i + 1
            }
            colnames(stat@averages) <- variable
            colnames(stat@SEM) <- colnames(stat@averages)
            
            return(stat)
          })

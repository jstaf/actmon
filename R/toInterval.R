# collapse data to a certain time interval, aggregating by mean or sum
setGeneric("toInterval", function(obj, target, units, aggregateBy) {standardGeneric("toInterval")})
setMethod("toInterval", signature = "DAM",
          definition = function(obj, target,
                                units = c("seconds", "minutes", "hours"),
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

# This sets up the DAM experiment class and its native methods.

setClass("DAM", slots = c(data = "data.frame", sample_info = "data.frame"))

# Arguments must be dataframes.
newExperiment <- function(dataFile = NULL, infoFile = NULL) {
  if (is.character(dataFile)) dataFile <- parseDAM(dataFile)
  if (is.character(infoFile)) infoFile <- read.csv(infoFile)
  new("DAM", data = dataFile, sample_info = infoFile)
}

# A more generic function to subset your data by whichever column you freaking
# want. Even works on vectors, so you can subset by multiple values of an
# attribute at a time.
setGeneric("byAttribute", function(obj, string, col) {standardGeneric("byAttribute")})
setMethod(f = "byAttribute", signature = "DAM",
          definition = function(obj, string, col) {
            #figure out which column in the info file to read from
            col <- as.character(col)
            numCol <- which(colnames(obj@sample_info) == col)

            # subset sampleinfo
            string <- as.character(string)
            idx <- which(obj@sample_info[, numCol] %in% string)
            obj@sample_info <- obj@sample_info[idx, ]

            # subset data
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, c(1:lightsCol, offsetIdx)]

            return(obj)
          })

# Equivalent to byAttribute(), but drops data by a value instead. Works on vectors.
setGeneric("dropAttribute", function(obj, string, col) {standardGeneric("dropAttribute")})
setMethod(f = "dropAttribute", signature = "DAM",
          definition = function(obj, string, col) {
            #figure out which column in the info file to read from
            col <- as.character(col)
            numCol <- which(colnames(obj@sample_info) == col)

            # subset sampleinfo
            string <- as.character(string)
            idx <- which(obj@sample_info[, numCol] %in% string)
            obj@sample_info <- obj@sample_info[-idx, ]

            # subset data
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, -offsetIdx]

            return(obj)
          })

# Use this function to mash experiments together. There is no restriction to
# doing this by timestamp, etc. - so be careful what you combine. The vial
# indices in the second experiment are relabeled to start where the first
# experiment stops.
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
              colnames(obj2@data)[lightsCol + 1:numVials] <- (last + 1):numVials
              
              # combine objects
              obj1@sample_info <- rbind(obj1@sample_info, obj2@sample_info)
              
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

# Determine measurement interval (in seconds)
setGeneric("getInterval", function(obj) {standardGeneric("getInterval")})
setMethod("getInterval", signature = "DAM",
          definition = function(obj) {
            idxDiff <- obj@data[10, 1] - obj@data[9, 1]
            return(as.numeric(difftime(obj@data[10, 2], obj@data[9, 2], units = "secs")) / idxDiff)
          })

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
            newDAM <- DAMobject[seq(1, length(DAMobject[, 1]) - scale, scale), ]
            newDAM <- setVals(newDAM, compressed)

            # Cleanup rownames/indices for future operations.
            newDAM$read_index <- 1:length(newDAM$read_index)
            rownames(newDAM) <- newDAM$read_index

            # Replace old object data
            obj@data <- newDAM

            return(obj)
          })

# Use this function to subset out a particular portion of an experiment (to clip
# off unused data from beginnning and end).
setGeneric("subsetTime", function(obj, startTime, expDuration, units) {
  standardGeneric("subsetTime")
  })
setMethod("subsetTime", signature = "DAM",
          definition = subsetTime <- function(obj, startTime = 0, expDuration = dim(obj)[1],
                                              units = c("seconds", "minutes", "hours")) {
            # Parse them arguments...
            units <- match.arg(units)
            startTime <- toSeconds(startTime, units)
            expDuration <- toSeconds(expDuration, units)
            interval <- getInterval(obj)
            
            # Fix bad values to aid indexing.
            if (startTime %% interval != 0) {
              warning("startTime is not a multiple of data interval, coercing to integer.")
              startTime <- floor(startTime / interval) * interval
            }
            if (expDuration %% interval != 0) {
              warning("expDuration is not a multiple of data interval, coercing to integer.")
              expDuration <- floor(expDuration / interval) * interval
            }
            
            # Okay subset out and return the data we want.
            toReturn <- (startTime / interval):((startTime / interval) + (expDuration / interval))
            obj@data <- obj@data[toReturn, ]
            return(obj)
          })

# List all attributes (column names) in the sample_info file.
setGeneric("listAttributes", function(obj) {standardGeneric("listAttributes")})
setMethod("listAttributes", signature = "DAM",
          definition = function(obj) {
            return(colnames(obj@sample_info))
          })

# A convenience method to show the possible values of an attribute.
setGeneric("listAttribVals", function(obj, attribute) {standardGeneric("listAttribVals")})
setMethod("listAttribVals", signature = "DAM",
          definition = function(obj, attribute) {
            attribute <- as.character(attribute)
            if (!any(colnames(obj@sample_info) == attribute)) stop("Attribute not found.")
            col <- which(colnames(obj@sample_info) == attribute)
            vals <- unique(obj@sample_info[, col])
            # reorder factor in the order that its values appear in sampleinfo
            if (is.factor(vals)) {
              vals <- factor(vals, levels = vals)
            }
            return(vals)
          })

# Applies the function isAsleep to the dataset.
setGeneric("calcSleep", function(obj) {standardGeneric("calcSleep")})
setMethod("calcSleep", signature = "DAM",
          definition = function(obj) {
            rate <- getInterval(obj)
            if (rate < 300) {
              warning("Data rate is less than 5 min/reading, aggregating by sum.")
              obj <- toInterval(obj, 5, units = "minutes", aggregateBy = "sum")
            } else if (rate > 300) {
              warning("Data rate is greater than 5 minutes per reading, sleep may be underestimated.")
            }

            vals <- getVals(obj@data)
            vals <- apply(vals, c(1, 2), isAsleep)
            obj@data <- setVals(obj@data, vals)

            return(obj)
          })

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

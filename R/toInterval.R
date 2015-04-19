# This function compresses data down to whatever data rate specified.

# Only use this after performing any data pre-processing (like computing sleep)
# you might want to do.

toInterval <- function(DAMobject, target,
                       units = c("seconds", "minutes", "hours"),
                       aggregateBy = c("sum", "average")) {
  #Parse args
  units <- match.arg(units)
  target <- toSeconds(target, units)
  aggregateBy <- match.arg(aggregateBy)

  # Check that we are not artificially increasing data resolution.
  interval <- getInterval(DAMobject)
  if (interval > target) {
    stop("End interval cannot be smaller than start interval.")
  }

  # Okay now scale the data
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
  setVals(newDAM, compressed)

  # Cleanup rownames/indices for future operations.
  newDAM$read_index <- 1:length(newDAM$read_index)
  rownames(newDAM) <- newDAM$read_index

  return(newDAM)
}

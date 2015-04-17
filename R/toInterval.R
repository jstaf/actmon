# This function compresses data down to whatever data rate specified.

# Only use this after performing any data pre-processing (like computing sleep)
# you might want to do.

toInterval <- function(DAMobject, target,
                       units = c("seconds", "minutes", "hours")) {
  # Convert target to seconds
  if (units == "minutes") {
    target <- target * 60
  } else if (units == "hours") {
    target <- target * 3600
  }

  # Check that we are not artificially increasing data resolution.
  interval <- getInterval(DAMobject)
  if (interval > target) {
    stop("End interval cannot be smaller than start interval")
  }

  # Okay now scale the data
  scale <- target / interval
  countsMatrix <- getVals(DAMobject)
  numInt <- length(countsMatrix[,1]) %/% scale
  remainder <- length(countsMatrix[,1]) %% scale


  setVals(DAMobject, countsMatrix)
}

toInterval(DAM, 60, "minutes")

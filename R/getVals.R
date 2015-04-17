# Retrieve values from a DAM dataframe as a matrix.

getVals <- function(DAMobject) {
  # Retrieve first index of actual data.
  idx <- which(colnames(DAMobject) == "light_status") + 1
  # Now retrieve everything after that index.
  return(as.matrix(DAMobject[,idx:length(colnames(DAMobject))]))
}

# Replace the values of a DAM dataframe with those you specify here.

setVals <- function(DAMobject, matrix) {
  # Retrieve first index of actual data.
  idx <- which(colnames(DAMobject) == "light_status") + 1
  DAMobject[,idx:length(colnames(DAMobject))] <- matrix
}

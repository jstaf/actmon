# A class that holds several common statistics for a dataset.

setClass("DAMstats", slots = c(averages = "data.frame", SEM = "data.frame"))

newStats <- function(DAM = NULL, attribute) {
  if (class(DAM) != "DAM") stop("You must specify a valid DAM object.")
  attribute <- as.character(attribute)

  if (!any(colnames(DAM@sample_info) == attribute)) stop("Attribute not found.")
  colOfInterest <- which(colnames(DAM@sample_info) == attribute)

  numFactor <- length(unique(DAM@sample_info[, colOfInterest]))

  df <- as.data.frame(matrix(nrow = dim(DAM@data)[, 1], ncol = numFactor))
  return(new("DAMstats", averages = df, SEM = df))
}

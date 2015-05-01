# grab the indices of a vector where a string of zeros starts.

#vector <- sleep@data$light_status

findZeros <- function(vector) {
  zeroStarts <- rep(NA, length(vector))
  for (i in 2:length(vector)) {
    j <- 1
    if ((vector[i] == 0) && (vector[i - 1] != vector[i])) {
      zeroStarts[j]
      j <- j + 1
    }
  }
  zeroStarts <- na.omit(zeroStarts)
  return(zeroStarts)
}

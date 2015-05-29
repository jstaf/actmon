#' Find changes in a vector
#' 
#' Find indices of which values in a vector differed by more than a certain
#' delta
#' 
#' @param vector
#' @param delta
#'   
#' @return indices which changed more than delta
#' 
whichChanged <- function(vector, delta) {
  if (length(vector) >= 2) {
    changed <- rep(FALSE, length(vector))
    for (i in 2:length(vector)) {
      if (abs(vector[i] - vector[i - 1]) > delta) {
        changed[i] <- TRUE
      }
    }
  } else {
    stop("Vector length must be greater than or equal to 2.")
  }
  return(which(changed))
}

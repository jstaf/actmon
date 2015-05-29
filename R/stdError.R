#' Calculate standard error of the mean
#' 
#' Computes standard error of a vector
#' 
#' @param vector 
#'
#' @return sem
#'
stdError <- function(vector) {
  sd(vector) / sqrt(length(vector))
}

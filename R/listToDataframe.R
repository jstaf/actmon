#' Convert a list to a dataframe
#' 
#' Converts a list to a datframe. Each element of the list gets its own column. 
#' If elements of the list are of different lengths, NA's will be added to the
#' end of each column.
#' 
#' @param list A list
#'   
#' @return Returns a dataframe
#' @export
#' 
listToDataframe <- function(list) {
  maxLen <- max(unlist(lapply(list, length)))
  out <- as.data.frame(matrix(data = NA, ncol = length(list), nrow = maxLen))
  colnames(out) <- names(list)
  for (i in 1:length(list)) {
    out[1:length(list[[i]]), i] = list[[i]]
  }
  return(out)
}

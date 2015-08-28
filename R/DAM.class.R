# This sets up the DAM experiment class and its native methods.

#' DAM experiment object
#' 
#' This is an S4 object designed to hold the output produced by Trikinetics' 
#' Drosophila Activity Monitor in its native format as well as its metadata.
#' 
#' @slot data A dataframe containing the output of a Drosophila Activity Monitor
#'   experiment.
#' @slot sample_info A dataframe that contains the metadata for an activity
#'   monitor experiment, with one entry for every vial present.
#'   
#' @export
setClass("DAM", slots = c(data = "data.frame", sample_info = "data.frame"))


#' Create a DAM experiment
#' 
#' @param dataFile A string containing the path to a valid DAM output .txt file.
#' @param infoFile A string containing the path to a .csv (comma-separated 
#'   values) file that you have created for the experiment. The only 
#'   requirements for this file is that it must contain a 1-line header (these 
#'   are the attribute names), and the first column must contain the vial
#'   numbers used in the experiment (typically 1 through 32).
#'   
#' @return A DAM S4 object.
#' @export
#' 
#' @examples
#' \dontrun{
#' DAM <- newExperiment(dataFile = "/path/to/DAM_output.txt", infoFile = "/path/to/metadata.csv")
#' }
newExperiment <- function(dataFile = NULL, infoFile = NULL) {
  if (is.character(dataFile)) dataFile <- parseDAM(dataFile)
  if (is.character(infoFile)) infoFile <- read.csv(infoFile)
  
  # convert factor ordering to the order in which they appear in the dataset
  for (colNum in 2:dim(infoFile)[2]) {
    if (is.factor(infoFile[, colNum])) {
      infoFile[, colNum] <- factor(infoFile[, colNum], levels = unique(infoFile[, colNum]))
    }
  }
  new("DAM", data = dataFile, sample_info = infoFile)
}

# create a new "empty template" for replacement
setGeneric("newTemplate", function(obj, long) {standardGeneric("newTemplate")})
setMethod("newTemplate", signature = c("DAM", "numeric"),
          definition = function(obj, long) {
            template <- obj@data[1:long, ]
            colnames(template) <- colnames(obj@data)
            
            # blindly copy over metadata without processing
            idx <- which(colnames(template) == "light_status")
            template[1:long, 1:idx] <- obj@data[1:long, 1:idx]
            
            return(new("DAM", data = template, sample_info = obj@sample_info))
          })

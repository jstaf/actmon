#' Export a DAM experiment
#' 
#' This is a convenience method to export your data for use with other software.
#' Creates two files, a .txt file with the raw data for your dataset, and a .csv
#' file with all of its metadata. The .txt file will be in the original 
#' TriKinetics format. NOTE: if you have performed any sleep calculations on a 
#' dataset, it will confuse other software packages. Export only activity counts
#' if you wish to use a dataset in other DAM applications.
#' 
#' @param obj A valid DAM S4 object (created by \code{\link{newExperiment}})
#' @param filename The filename MINUS THE EXTENSION that you wish to export your
#'   data to. Note that file extensions will automatically added.
#'   
#' @return Writes two files to your current path: one with an experiment's raw 
#'   data, and another with its metadata.
#' @export
#' 
#' @examples
#' \dontrun{
#' exportDataset(DAM_DD, "baseFilename")
#' }
setGeneric("exportDataset", function(obj, filename) {standardGeneric("exportDataset")})
setMethod("exportDataset", signature = "DAM", 
          definition = function(obj, filename) {
            write.table(obj@data, 
                        file = paste(filename, ".txt", sep = ""),
                        quote = FALSE, sep = "\t", row.names = FALSE, col.names = FALSE)
            write.csv(obj@sample_info,
                      file = paste(filename, ".csv", sep = ""),
                      quote = FALSE, row.names = FALSE)
          })

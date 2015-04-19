# This sets up the DAM experiment class

setClass("DAM",
         slots = c(data = "data.frame",
                   sample_info = "data.frame"))

newExperiment <- function(dataFile = NULL, infoFile = NULL) {
  new("DAM", data = dataFile, sample_info = infoFile)
}

# A method to combine experiments
# setMethod("catExperiments", signature = "DAM",
#           #valueClass = "DAM",
#           valueClass = "data.frame",
#           definition = function(exp1, exp2) {
#             exp1@data
#             }
#           )

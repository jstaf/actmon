# This sets up the DAM experiment class and its native methods.

setClass("DAM", slots = c(data = "data.frame", sample_info = "data.frame"))

# Arguments must be dataframes.
newExperiment <- function(dataFile = NULL, infoFile = NULL) {
  if (is.character(dataFile)) dataFile <- parseDAM(dataFile)
  if (is.character(infoFile)) infoFile <- read.csv(infoFile)
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

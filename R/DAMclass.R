# This sets up the DAM experiment class and its native methods.

setClass("DAM", slots = c(data = "data.frame", sample_info = "data.frame"))

newExperiment <- function(dataFile = NULL, infoFile = NULL) {
  if (is.null(dataFile) || is.null(infoFile)) {
    warning("One or more arguments is missing from DAM constructor.")
  }
  new("DAM", data = dataFile, sample_info = infoFile)
}

# Subset out flies by sex.
setGeneric("bySex", function(obj, sex) {standardGeneric("bySex")})
setMethod(f = "bySex", signature = "DAM",
          definition = function(obj, sex = c("female", "male")) {
            sex <- match.arg(sex)
            if (sex == "female") {
              sex <- 0
            } else {
              sex <- 1
            }

            # Some really dirty subsetting....
            idx <- which(obj@sample_info$sex == sex)
            obj@sample_info <- obj@sample_info[idx, ]
            lightsCol <- which(colnames(obj) == "light_status")
            offsetIdx <- lightsCol + 1 + idx
            obj@data <- obj@data[c(1:lightsCol, idx), ]

            return(obj)
          })



# A more generic function to subset your data by whichever column you freaking
# want. Even works on vectors, so you can subset by multiple values of an
# attribute at a time.
setGeneric("byAttribute", function(obj, string, col) {standardGeneric("byAttribute")})
setMethod(f = "byAttribute", signature = "DAM",
          definition = function(obj, string, col) {
            #figure out which column in the info file to read from
            col <- as.character(col)
            numCol <- which(colnames(obj@sample_info) == col)
            
            # subset sampleinfo
            string <- as.character(string)
            idx <- which(obj@sample_info[, numCol] %in% string)
            obj@sample_info <- obj@sample_info[idx, ]
            
            # subset data
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, c(1:lightsCol, offsetIdx)]
            
            return(obj)
          })

# Equivalent to byAttribute(), but drops data by a value instead. Works on vectors.
setGeneric("dropAttribute", function(obj, string, col) {standardGeneric("dropAttribute")})
setMethod(f = "dropAttribute", signature = "DAM",
          definition = function(obj, string, col) {
            #figure out which column in the info file to read from
            col <- as.character(col)
            numCol <- which(colnames(obj@sample_info) == col)
            
            # subset sampleinfo
            string <- as.character(string)
            idx <- which(obj@sample_info[, numCol] %in% string)
            obj@sample_info <- obj@sample_info[-idx, ]
            
            # subset data
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, -offsetIdx]
            
            return(obj)
          })

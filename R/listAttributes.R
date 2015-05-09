# List all attributes (column names) in the sample_info file.
setGeneric("listAttributes", function(obj) {standardGeneric("listAttributes")})
setMethod("listAttributes", signature = "DAM",
          definition = function(obj) {
            return(colnames(obj@sample_info))
          })

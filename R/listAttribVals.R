# A convenience method to show the possible values of an attribute.
setGeneric("listAttribVals", function(obj, attribute) {standardGeneric("listAttribVals")})
setMethod("listAttribVals", signature = "DAM",
          definition = function(obj, attribute) {
            attribute <- as.character(attribute)
            if (!any(colnames(obj@sample_info) == attribute)) stop("Attribute not found.")
            col <- which(colnames(obj@sample_info) == attribute)
            vals <- unique(obj@sample_info[, col])
            # reorder factor in the order that its values appear in sampleinfo
            if (is.factor(vals)) {
              vals <- factor(vals, levels = vals)
            }
            return(vals)
          })

#' Get attribute values
#' 
#' A convenience method to show the possible values of an attribute. These are
#' the values from a column in the original .csv metadata file.
#' 
#' @seealso \code{\link{listAttributes}}
#'   
#' @param obj A valid DAM S4 object
#' @param attribute A string that matches
#'   
#' @return A string of attribute values remaining in a dataset.
#' @export
#' 
#' @examples
#' listAttribVals(DAM_DD, "genotype")
setGeneric("listAttribVals", function(obj, attribute) {standardGeneric("listAttribVals")})
setMethod("listAttribVals", signature = "DAM",
          definition = function(obj, attribute) {
            attribute <- as.character(attribute)
            if (!any(colnames(obj@sample_info) == attribute)) { 
              stop("Attribute not found.")
            }
            col <- which(colnames(obj@sample_info) == attribute)
            vals <- unique(obj@sample_info[, col])
            # reorder factor in the order that its values appear in sampleinfo
            if (is.factor(vals)) {
              vals <- factor(vals, levels = vals)
            }
            return(vals)
          })

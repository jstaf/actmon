#' Get the attributes of a dataset
#'
#' List all attributes (column names) from a dataset's metadata.
#'
#' @param obj A valid DAM S4 object.
#'
#' @return A vector of all attribute names in a dataset.
#' @export
#'
#' @examples
#' listAttributes(DAM_DD)
setGeneric("listAttributes", function(obj) {standardGeneric("listAttributes")})
setMethod("listAttributes", signature = "DAM",
          definition = function(obj) {
            return(colnames(obj@sample_info))
          })

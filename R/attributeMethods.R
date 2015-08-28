# A couple methods to allow easier work with attributes

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

#' Get attribute values
#' 
#' A convenience method to show the possible values of an attribute. These are 
#' the values from a column in the original .csv metadata file.
#' 
#' @seealso \code{\link{listAttributes}}
#'   
#' @param obj A valid DAM S4 object
#' @param attribute A string that matches one of the column names from the
#'   original sample info .csv file
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
            names(vals) <- 1:length(vals)
            
            return(vals)
          })

#' Change attribute order
#' 
#' This is a convenience function that allows easy reordering of attribute 
#' values for plotting (does not affect data analysis). Bars in the wrong order?
#' Use this function.
#' 
#' @param obj A valid DAM S4 object
#' @param attribute A string that matches one of the column names from the 
#'   original sample info .csv file
#' @param order A vector with the new order of values for the attribute. Can be
#'   either a character vector with the 
#'   
#' @return Returns a DAM object with the attribute of concern reordered
#' @export
#' 
#' @seealso To see the current attribute order, use \code{\link{listAttribVals}}
#'   
#' @examples 
#' # reorder by number
#' dat <- data(DAM_DD)
#' listAttribVals(DAM_DD, "genotype")
#' dat <- orderAttribVals(DAM_DD, "genotype", c(3, 2, 1))
#' listAttribVals(DAM_DD, "genotype")
#' 
#' # reorder by values
#' dat <- data(DAM_DD)
#' listAttribVals(DAM_DD, "genotype")
#' dat <- orderAttribVals(DAM_DD, "genotype", c("experimental", "control B", "control A"))
#' listAttribVals(DAM_DD, "genotype")
setGeneric("orderAttribVals", function(obj, attribute, order) {
  standardGeneric("orderAttribVals")})
# reorder with numeric indices
setMethod("orderAttribVals", signature = c("DAM", "character", "numeric"),
          definition = function(obj, attribute, order) {
            if (!any(colnames(obj@sample_info) == attribute)) { 
              stop("Attribute not found.")
            }
            col <- which(colnames(obj@sample_info) == attribute)
            if (length(unique(obj@sample_info[, col])) != length(order)) {
              stop("The length of the 'order' vector must be equal to the number of unique attribute elements.")
            }
            obj@sample_info[, col] <- factor(obj@sample_info[, col],
                                             levels = unique(obj@sample_info[, col])[order])
            return(obj)
          })
# reorder with the direct level names
setMethod("orderAttribVals", signature = c("DAM", "character", "character"),
          definition = function(obj, attribute, order) {
            if (!any(colnames(obj@sample_info) == attribute)) { 
              stop("Attribute not found.")
            }
            col <- which(colnames(obj@sample_info) == attribute)
            if (length(unique(obj@sample_info[, col])) != length(order)) {
              stop("The length of the 'order' vector must be equal to the number of unique attribute elements.")
            }
            obj@sample_info[, col] <- factor(obj@sample_info[, col],
                                             levels = order)
            return(obj)
          })

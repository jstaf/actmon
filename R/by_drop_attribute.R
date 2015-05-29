#' Subset data by an attribute
#' 
#' A more generic function to subset your data by whichever column you  want.
#' Even works on vectors, so you can subset by multiple values of an attribute
#' at a time. To remove data by an attribute, see \code{\link{dropAttribute}}.
#' 
#' @seealso \code{\link{dropAttribute}}
#' 
#' @param obj A valid DAM object (created by \code{\link{newExperiment}}).
#' @param string Attribute values to include. Can be a vector.
#' @param col Which column of attribute values should be used.
#'   
#' @return Returns a DAM S4 object.
#' @export
#' 
#' @examples
#' listAttribVals(DAM_DD, "genotype")
#' returnedDAM <- byAttribute(DAM_DD, "experimental")
#' listAttribVals(returnedDAM, "genotype")
#' 
#' listAttribVals(DAM_DD, "genotype")
#' returnedDAM <- byAttribute(DAM_DD, c("control A", "control B"))
#' listAttribVals(returnedDAM, "genotype")
#' 
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

#' Remove data by an attribute
#'
#' Equivalent to \code{\link{byAttribute}}, but drops data by a value instead. Works on vectors.
#'
#' @seealso \code{\link{byAttribute}}
#'
#' @param obj A valid DAM object (created by \code{\link{newExperiment}}).
#' @param string Attribute values to include. Can be a vector.
#' @param col Which column of attribute values should be used.
#'   
#' @return Returns a DAM S4 object.
#' @export
#' 
#' @examples
#' listAttribVals(DAM_DD, "genotype")
#' returnedDAM <- byAttribute(DAM_DD, "experimental")
#' listAttribVals(returnedDAM, "genotype")
#' 
#' listAttribVals(DAM_DD, "genotype")
#' returnedDAM <- byAttribute(DAM_DD, c("control A", "control B"))
#' listAttribVals(returnedDAM, "genotype")
#' 
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

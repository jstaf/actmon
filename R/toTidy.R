#' Convert a dataset to TidyR format
#' 
#' This method converts a data and all its associated metadata to the TidyR 
#' format. See 
#' \link{http://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html}
#' for details.
#' 
#' @param obj A DAM S4 experiment
#'   
#' @return Returns a dataframe in TidyR format.
#' @export
#' 
#' @examples
#' tidy <- toTidy(DAM_DD)
setGeneric("toTidy", def = function(obj) {standardGeneric("toTidy")})
setMethod("toTidy", signature = "DAM",
          definition = function(obj) {
            # reshape obj@data into long format
            excl <- which(colnames(obj@data) %in% c("status", "extra_readings", "unused", "unused.1", "unused.2", "unused.3", "light_status"))
            dat <- obj@data[,-excl]
            dat <- reshape2::melt(dat, 
                                  id.vars = c("read_index", "read_time"), 
                                  na.rm = TRUE)
            colnames(dat)[3] <- "vial_num"
            
            # add metadata for our attribute of interest
            idx <- match(dat$vial_num, obj@sample_info[,1])
            cols <- colnames(obj@sample_info)[-1]
            meta <- obj@sample_info[idx, cols]
            rownames(meta) <- NULL
            dat <- cbind(meta, dat)
            
            # required if the metadata only has two columns
            if (length(colnames(obj@sample_info)) == 2) {
              colnames(dat)[1] <- colnames(obj@sample_info)[2]
            }
            
            return(dat)
          })

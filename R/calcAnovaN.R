#' Calculate statistics for a dataset
#' 
#' A 1-way ANOVA is performed across an attribute on data with a single 
#' timepoint. A 2-way ANOVA is performed across an attribute on data with 
#' multiple timepoints (this will take quite awhile if done on data with 
#' an extreme number of timepoints). Note that a 1-way ANOVA is statistically
#' identical to a two-sample t-test when there are only two sample groups.
#' 
#' @param obj A valid DAM object (created by \code{\link{newExperiment}}
#' @param attribute Which attribute stats should be calculated against.
#'   
#' @return Returns 
#' @export
#' 
#' @examples
#' activity <- toInterval(DAM_DD, 12, units = "hours", aggregateBy = "sum")
#' activity <- toAvgDay(activity)
#' statsAct <- calcStats(activity, "genotype")
setGeneric("calcANOVA", function(obj, attribute) {standardGeneric("calcANOVA")})
setMethod("calcANOVA", signature = "DAM",
          definition = function(obj, attribute){
            # okay do a one-way anova if there's only one row
            if (length(obj@data[, 1]) == 1) {
              calcAnova1(obj, 1, attribute)
            } else {
              calcAnova2(obj, attribute)
            }
            return()
          })

# compute 1-way ANOVA, return which comparisons gave p <= 0.05
setGeneric("calcAnova1", function(obj, row, attribute) {standardGeneric("calcAnova1")})
setMethod("calcAnova1", signature = c("DAM", "numeric", "character"), 
          definition = function(obj, row, attribute) {
            dat <- getVals(obj@data)[row, ]
            attributes <- obj@sample_info[, attribute]
            if (length(dat) != length(attributes)) {
              stop("Number of conditions is unequal to the data length.")
            }
            model <- aov(dat ~ attributes)
            print(summary(model))

            return(model)
          })

# this is the internal function used to call 2 way anovas
setGeneric("calcAnova2", function(obj, attribute) {standardGeneric("calcAnova2")})
setMethod("calcAnova2", signature = c("DAM", "character"), 
          definition = function(obj, attribute) { 
            melted <- toTidy(obj)
            col <- which(colnames(melted) == attribute)
            colnames(melted)[col] <- "attr"
            melted$read_index <- as.factor(melted$read_index)
            
            # okay compute the model
            model <- with(melted, aov(value ~ attr * read_index))
            print(summary(model))
            
            return(model)
          })

#' Run a Tukey HSD post-hoc test
#'
#' This is a convenience function designed to print out all of the significant 
#' comparisons (p < 0.05) from an ANOVA model
#'
#' @param aov An ANOVA model
#'  
#' @return Returns all of the significant 
#' @export
#'
setGeneric("calcTukeyHSD", function(aov) {standardGeneric("calcTukeyHSD")})
setMethod("calcTukeyHSD", signature = c("aov"), 
          definition = function(aov) {
            pvals <- TukeyHSD(aov)
            # take only pvals <= 0.05
            pvals <- pvals[pvals[, "p adj"] <= 0.05, ]
            return(pvals)
          })


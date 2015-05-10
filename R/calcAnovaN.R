# compute 1-way ANOVA, return which comparisons gave p <= 0.05
setGeneric("calcAnova1", function(obj, row, attribute) {standardGeneric("calcAnova1")})
setMethod("calcAnova1", signature = c("DAM", "numeric", "character"), 
          definition = function(obj, row, attribute) {
            dat <- getVals(obj@data)[1, ]
            conditions <- obj@sample_info[, attribute]
            if (length(dat) != length(conditions)) {
              stop("Number of conditions is unequal to the data length.")
            }
            model <- aov(dat ~ conditions)
            # this is dirty - perform tukey hsd if p<0.05
            if (summary(model)[[1]][["Pr(>F)"]][[1]] <= 0.05) {
              # grab p values
              pvals <- TukeyHSD(model)[[1]][, 4]
              pvals <- pvals[pvals <= 0.05]
              # not even sure this is necessary, but catch any 0-length pval vectors
              if (length(pvals) == 0) {
                pvals <- NA
              }
            } else {
              pvals <- NA
            }
            return(pvals)
          })

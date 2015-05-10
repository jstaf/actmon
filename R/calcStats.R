# Iterate through possible variations of an attribute in your dataset,
# calculating averages/standard error of the mean/stats for each. Returns a
# DAMstats object.
setGeneric("calcStats", function(obj, attribute) {standardGeneric("calcStats")})
setMethod("calcStats", signature = "DAM",
          definition = function(obj, attribute){
            # retrieve all values
            stat <- newStats(obj, attribute)
            variable <- listAttribVals(obj, attribute)
            
            # iterate through values and calc avg/sem
            i <- 1
            for (var in variable) {
              temp <- getVals(byAttribute(obj, var, attribute)@data)
              stat@averages[, i] <- rowMeans(temp)
              stat@SEM[, i] <- apply(as.matrix(temp), 1, stdError)
              i <- i + 1
            }
            colnames(stat@averages) <- variable
            colnames(stat@SEM) <- colnames(stat@averages)
            
            # okay do a one-way anova if there's only one row
            if (length(obj@data[, 1]) == 1) {
              anova_results <- calcAnova1(getVals(obj@data)[1, ],
                                          obj@sample_info[,attribute])
            }
            
            return(stat)
          })

# compute 1-way ANOVA, return which comparisons gave p <= 0.05
calcAnova1 <- function(dat, conditions) {
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
}

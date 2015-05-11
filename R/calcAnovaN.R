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
            print(summary(model))
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
            print(pvals)
            return(pvals)
          })

setGeneric("calcAnova2", function(obj, attribute) {standardGeneric("calcAnova2")})
setMethod("calcAnova2", signature = c("DAM", "character"), 
          definition = function(obj, attribute) { 
            # format data to be useable
            dat <- getVals(obj@data)
            conditions <- obj@sample_info[, attribute]
            time <- obj@data$read_index
            dimnames(dat) <- list(time, conditions)
            melted <- reshape2::melt(dat)
            colnames(melted) <- c("index", "attribute", "value")
            melted$index <- as.factor(melted$index)
            
            pvals <- NA
            # okay compute the model
            model <- with(melted, aov(value ~ attribute * index))
            print(summary(model))
            if (length(unique(melted$index)) <= 6) {
              pvals <- do.call(rbind, TukeyHSD(model))
              # take only pvals <= 0.05
              pvals <- pvals[pvals[, "p adj"] <= 0.05, ]
              print(pvals)
            }
            return(pvals)
          })


library(plyr)

# plot a single vector of values using the metadata from the object it was produced by
vectorPlot <- function(vector, obj, attribute) {
  vector[27] = 0
  df <- data.frame(vialNum = names(vector),
                   attr = obj@sample_info[, which(colnames(obj@sample_info) == attribute)],
                   values = vector)
  plotData <- plyr::ddply(df, .(attr), plyr::summarise,
                          AVG = mean(values), SEM = stdError(values))

  gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = attr,
                                               y = AVG,
                                               ymin = AVG - SEM,
                                               ymax = AVG + SEM,
                                               fill = attr)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) + #, position = "dodge") +
    ggplot2::geom_errorbar(width = 0.4) + #, position = "dodge") +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme_bw()
  return(gg)
  
#   ## copy pasted from calc anova1
#   model <- aov(df$values ~ df$attribute)
#   print(summary(model))
#   # this is dirty - perform tukey hsd if p<0.05
#   if (summary(model)[[1]][["Pr(>F)"]][[1]] <= 0.05) {
#     # grab p values
#     pvals <- TukeyHSD(model)[[1]][, 4]
#     pvals <- pvals[pvals <= 0.05]
#     # not even sure this is necessary, but catch any 0-length pval vectors
#     if (length(pvals) == 0) {
#       pvals <- NA
#     }
#   } else {
#     pvals <- NA
#   }
#   print(pvals)
}

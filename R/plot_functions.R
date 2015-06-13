# new plotting functions that use a sensible data format

#' Calculate average values by attribute
#' 
#' This function is used to calculate the average values 
#' (sleep/activity/whatever) of your data by an attribute. For instance, this
#' could be used to calculate the average sleep values for each genotype.
#' 
#' @param obj A valid DAM S4 object (created by \code{\link{newExperiment}})
#' @param attribute Which attribute you would like to use to analyze your data 
#'   by, e.g. "genotype"
#'   
#' @return A matrix of values with the average and standard error for each attribute 
#'   category
#' @export
#' 
#' @examples
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' calcAttribMeans(sleep, "genotype")
setGeneric("calcAttribMeans", def = function(obj, attribute) {standardGeneric("calcAttribMeans")})
setMethod("calcAttribMeans", signature = c("DAM", "character"),
          definition = function(obj, attribute) {
            dat <- toTidy(obj)
            plotData <- plyr::ddply(dat, c(attribute, "read_index"), plyr::summarise,
                                    AVG = mean(value), SEM = stdError(value))
            # avoid confusing ggplot... use a standard column name
            colnames(plotData)[1] <- "attr"
            
            # catch datasets with length of 1
            if (length(plotData$read_index) > 1) {
              # convert read_index to hours
              plotData$read_index <- plotData$read_index * getInterval(obj) / 3600
            }
            
            # reorder factor levels to order in which they appear
            plotData$attr <- factor(plotData$attr, levels = unique(listAttribVals(obj, attribute)))
            return(plotData)
          })

#' Create a bar plot of an experiment
#' 
#' Create a bar plot from calculated statistics (plots data means by attribute).
#' Error bars represent the standard error of the mean. Plot is automatically 
#' faced if using multiple timepoints. Do not use this function for datasets 
#' with a large number of timepoints (use \code{\link{linePlot}} instead). All 
#' ggplot2 functions will work on the resulting plot. If you want to plot a 
#' vector of numeric values (like survival time or # of sleep bouts), supply the
#' "vector" argument (of the values you wish to use) along with the
#' obj/attribute the values were originally generated from.
#' 
#' @inheritParams linePlot
#' @param vector A vector of values, with one for each animal in the experiment
#'   
#' @return Returns a ggplot2 plot object
#' @export
#' 
#' @examples
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' sleep <- toInterval(sleep, 12, units = "hours", aggregateBy = "average")
#' sleep <- toAvgDay(sleep)
#' barPlot(sleep, "genotype")
#' 
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' bouts <- calcMeanBout(sleep)
#' barPlot(sleep, "genotype", vector = bouts)
setGeneric("barPlot", def = function(obj, attribute, ..., vector) {standardGeneric("barPlot")})
setMethod("barPlot", signature = c("DAM", "character"), 
          definition = function(obj, attribute) {
            plotData <- calcAttribMeans(obj, attribute)
            
            # create the plot
            gg <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = attr,
                                                                y = AVG,
                                                                ymin = AVG - SEM,
                                                                ymax = AVG + SEM,
                                                                fill = attr)) +
              ggplot2::facet_wrap(~read_index) +
              ggplot2::geom_bar(stat = "identity", width = 0.8) +
              ggplot2::geom_errorbar(width = 0.4) +
              ggplot2::guides(fill = FALSE) + ggplot2::xlab("") + # erase redundant labels
              ggplot2::theme_bw() +
              ggplot2::theme(text = ggplot2::element_text(size = 14))
            return(gg)
          })

# an extra "bonus" method to plot vectors of data generated from a DAM object like sleep bouts
setMethod("barPlot", signature = c("DAM", "character", "numeric"),
          definition = function(obj, attribute, vector) {
            df <- data.frame(vialNum = names(vector),
                             attr = obj@sample_info[, which(colnames(obj@sample_info) == attribute)],
                             values = vector)
            df$attr <- factor(df$attr, levels = unique(df$attr))
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
          })


#' Create a line plot of an experiment
#'
#' Creates a line plot of the data. Useful for datasets with a lot of 
#' timepoints. Line datapoints represent the mean for that attribute. The 
#' lightly colored regions represent the standard error of the mean. Grey/white
#' areas of the background represent time periods where the lights were on/off.
#' Most useful for datasets with a large number of timepoints.
#' 
#' @seealso For datasets with only a few timepoints, use \code{\link{barPlot}} 
#'   instead.
#'
#' @param obj A valid DAM S4 object
#' @param attribute The attribute of the data you wish to examine (like "genotype")
#'
#' @return Returns a ggplot2 plot object
#' @export 
#'
#' @examples
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' sleep <- toInterval(sleep, 1, units = "hours", aggregateBy = "average")
#' linePlot(stat, "genotype") 
setGeneric("linePlot", def = function(obj, attribute) {standardGeneric("linePlot")})
setMethod("linePlot", signature = c("DAM", "character"),
          definition = function(obj, attribute) {
            plotData <- calcAttribMeans(obj, attribute)
            
            #TODO copy pasted... need to fix
            temp <- unique(na.omit(plotData$read_index))
            breakSeq <- seq(0, length(temp), (12 / (temp[2] - temp[1])))
            
            gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = read_index,
                                                         y = AVG,
                                                         ymin = AVG - SEM,
                                                         ymax = AVG + SEM,
                                                         fill = attr,
                                                         color = attr)) +
              ggplot2::geom_line() +
              ggplot2::geom_ribbon(alpha = 0.3, color = NA)
            
            # TODO: need to somehow specify this before the colored line calls
            # ugly, but it works... retrieve maximum y axis value
            maxVal <- ggplot2::ggplot_build(gg)$panel$ranges[[1]]$y.range[2]
            xlim <- c(1, temp[length(temp)])
            
            light_status <- (1 - obj@data$light_status) * maxVal            
            light_status[light_status == 0] <- NA
            gg <- gg + ggplot2::geom_ribbon(
              mapping = ggplot2::aes(x = plotData$read_index[1:length(light_status)], 
                                     y = 0, 
                                     ymin = 0, 
                                     ymax = light_status),
                                            alpha = 0.05, fill = "grey10", color = NA) +
              ggplot2::scale_y_continuous(limits = c(0, maxVal), expand = c(0,0)) +
              ggplot2::scale_x_continuous(breaks = breakSeq, labels = breakSeq,
                                          limits = xlim, expand = c(0,0)) +
              ggplot2::theme_bw() +
              ggplot2::theme(text = ggplot2::element_text(size = 14),
                             panel.border = ggplot2::element_rect(colour = "white"),
                             panel.grid.major = ggplot2::element_line(colour = "white"),
                             panel.grid.minor = ggplot2::element_line(colour = "white")) +
              ggplot2::geom_hline(yintercept = 0) +
              ggplot2::geom_vline(xintercept = 1)
            
            return(gg)
          })

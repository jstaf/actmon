#' Reshape data in preparation for plotting
#'
#' Performs the necessary data reshaping before a plot can be made
#'
#' @param statsObj 
#'
#' @return A list of meta and plotting data.
#'
setGeneric("prepPlot", function(statsObj) {standardGeneric("prepPlot")})
setMethod("prepPlot", signature = "DAMstats",
          definition = function(statsObj) {
            # reshape data into single, flat dataframe
            plotArray <- array(data = unlist(c(statsObj@averages, statsObj@SEM)),
                               dim = c(dim(statsObj@averages), 2))
            dimnames(plotArray) <- list(c(1:dim(statsObj@averages)[1]),
                                        as.character(colnames(statsObj@averages)),
                                        c("AVG", "SEM"))
            plotArray <- reshape2::melt(plotArray)
            colnames(plotArray) <- c("index", "attrib", "type", "value")
            plotData <- reshape2::dcast(plotArray, attrib + index ~ type)
            
            # create a new dfrm with meta
            meta <- data.frame(as.numeric(rownames(statsObj@averages)),
                               statsObj@read_time,
                               statsObj@light_status)
            colnames(meta) <- c("index", "read_time", "light_status")
            
            # an ugly way of passing data
            # TODO: bundle the meta and plotdata together
            return(list(meta, plotData))
          })

#' Create a bar plot
#' 
#' Create a bar plot from calculated statistics (plots data means by attribute).
#' Error bars represent the standard error of the mean. Plot is automatically
#' faced if using multiple timepoints. Do not use this function for datasets
#' with a large number of timepoints (use \code{\link{linePlot}} instead). All
#' ggplot2 functions will work on the resulting plot.
#' 
#' @param statsObj A DAMstats obj (created by \code{\link{calcStats}})
#'   
#' @return A ggplot2 plot object.
#' @export
#' 
#' @examples
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' sleep <- toInterval(sleep, 24, units = "hours", aggregateBy = "average")
#' sleep <- toAvgDay(sleep)
#' stat <- calcStats(sleep, "genotype")
#' barPlot(stat)
setGeneric("barPlot", function(statsObj) {standardGeneric("barPlot")})
setMethod("barPlot", signature = "DAMstats", 
          definition = function(statsObj) {
            dataList <- prepPlot(statsObj)
            meta <- dataList[[1]]
            plotData <- dataList[[2]]
            
            gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = attrib,
                                                         y = AVG,
                                                         ymin = AVG - SEM,
                                                         ymax = AVG + SEM,
                                                         fill = attrib)) +
              ggplot2::facet_wrap(~index) +
              ggplot2::geom_bar(stat = "identity", width = 0.8) + #, position = "dodge") +
              ggplot2::geom_errorbar(width = 0.4) + #, position = "dodge") +
              ggplot2::guides(fill = FALSE) +
              ggplot2::theme_bw()
            return(gg)
          })
          
#' Create a line plot of the data
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
#' @inheritParams barPlot
#'   
#' @return Returns a standard ggplot2 object.
#' @export
#' 
#' @examples
#' sleep <- dropDead(DAM_DD)
#' sleep <- calcSleep(sleep)
#' sleep <- toInterval(sleep, 1, units = "hours", aggregateBy = "average")
#' stat <- calcStats(sleep, "genotype")
#' linePlot(stat) 
setGeneric("linePlot", function(statsObj) {standardGeneric("linePlot")})
setMethod("linePlot", signature = "DAMstats", 
          definition = function(statsObj) {
            dataList <- prepPlot(statsObj)
            meta <- dataList[[1]]
            plotData <- dataList[[2]]
            
            # create labels for x axis
            # tolerate NAs
            temp <- na.omit(meta)
            hoursPerIdx <- as.numeric(difftime(temp[2, 2], temp[1, 2], units = "hours"))
            breakSeq <- seq(0, meta$index[length(meta$index)], 12 / hoursPerIdx)
            
            # create the actual plot and return it
            gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = index,
                                                         y = AVG,
                                                         ymin = AVG - SEM,
                                                         ymax = AVG + SEM,
                                                         fill = attrib,
                                                         color = attrib)) +
              ggplot2::geom_line() +
              ggplot2::geom_ribbon(alpha = 0.3, color = NA) +
              ggplot2::theme_bw() +
              ggplot2::scale_x_continuous(breaks = breakSeq, labels = breakSeq * hoursPerIdx)
            
            # TODO: need to somehow specify this before the colored line calls
            # ugly, but it works... retrieve maximum y axis value
            maxVal <- ggplot2::ggplot_build(gg)$panel$ranges[[1]]$y.range[2]
            meta$light_status <- (1 - meta$light_status) * maxVal            
            meta$light_status[meta$light_status == 0] <- NA
            gg <- gg + ggplot2::geom_ribbon(data = meta,
                                            ggplot2::aes(x = index, y = 0, ymin = 0, ymax = light_status),
                                            alpha = 0.05, fill = "grey10", color = NA) +
              ggplot2::scale_y_continuous(limits = c(0, maxVal))
            
            return(gg)
          })

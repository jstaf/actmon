
# formats data for plotting under either plotting method
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
            
            return(list(meta, plotData))
          })

# makes a barplot (dodged if using multiple timepoints)
barPlot <- function(dataList) {
  meta <- dataList[[1]]
  plotData <- dataList[[2]]
  
  gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = attrib,
                                               y = AVG,
                                               ymin = AVG - SEM,
                                               ymax = AVG + SEM,
                                               fill = attrib)) +
    ggplot2::geom_bar(stat = "identity", width = 0.8) +
    ggplot2::geom_errorbar(width = 0.4) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::theme_bw()
}

# makes a sweet line plot good for visualizing long timecourse data
linePlot <- function(dataList) {
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
}

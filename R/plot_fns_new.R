# new plotting functions that use a sensible data format

setGeneric("barPlot2", def = function(obj, attribute) standardGeneric("barPlot2"))
setMethod("barPlot2", signature = c("DAM", "character"), 
          definition = function(obj, attribute) {
            dat <- toTidy(obj)
            plotData <- plyr::ddply(dat, c(attribute, "read_index"), plyr::summarise,
                                    AVG = mean(value), SEM = stdError(value))
            # avoid confusing ggplot... use a standard column name
            colnames(plotData)[1] <- "attr"
            # convert read_index to hours
            plotData$read_index <- plotData$read_index * getInterval(obj) / 3600
            
            # create the plot
            gg <- ggplot2::ggplot(data = plotData, ggplot2::aes(x = attr,
                                                                y = AVG,
                                                                ymin = AVG - SEM,
                                                                ymax = AVG + SEM,
                                                                fill = attr)) +
              ggplot2::facet_wrap(~read_index) +
              ggplot2::geom_bar(stat = "identity", width = 0.8) +
              ggplot2::geom_errorbar(width = 0.4) +
              ggplot2::guides(fill = FALSE) + xlab("") + # erase redundant labels
              ggplot2::theme_bw()
            return(gg)
          })

# new plotting functions that use a sensible data format

setGeneric("prepPlot2", def = function(obj, attribute) {standardGeneric("prepPlot2")})
setMethod("prepPlot2", signature = c("DAM", "character"),
          definition = function(obj, attribute) {
            dat <- toTidy(obj)
            plotData <- plyr::ddply(dat, c(attribute, "read_index"), plyr::summarise,
                                    AVG = mean(value), SEM = stdError(value))
            # avoid confusing ggplot... use a standard column name
            colnames(plotData)[1] <- "attr"
            # convert read_index to hours
            plotData$read_index <- plotData$read_index * getInterval(obj) / 3600
            return(plotData)
          })

setGeneric("barPlot2", def = function(obj, attribute) {standardGeneric("barPlot2")})
setMethod("barPlot2", signature = c("DAM", "character"), 
          definition = function(obj, attribute) {
            plotData <- prepPlot2(obj, attribute)
            
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

setGeneric("linePlot2", def = function(obj, attribute) {standardGeneric("linePlot2")})
setMethod("linePlot2", signature = c("DAM", "character"),
          definition = function(obj, attribute) {
            plotData <- prepPlot2(obj, attribute)
            
            #TODO copy pasted... need to fix
            temp <- na.omit(meta)
            hoursPerIdx <- as.numeric(difftime(temp[2, 2], temp[1, 2], units = "hours"))
            breakSeq <- seq(0, meta$index[length(meta$index)], 12 / hoursPerIdx)
            
            #TODO copy pasted... need to fix
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

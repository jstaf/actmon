# A class that holds several common statistics for a dataset.

setClass("DAMstats", slots = c(attribute = "character", averages = "data.frame",
                               SEM = "data.frame", read_time = "POSIXct",
                               light_status = "integer", anova_results = "numeric"))

newStats <- function(DAM = NULL, attributeBy) {
  numFactor <- length(listAttribVals(DAM, attributeBy))
  df <- as.data.frame(matrix(nrow = dim(DAM@data)[1], ncol = numFactor))
  
  return(new("DAMstats", attribute = attributeBy, averages = df, SEM = df,
             read_time = DAM@data$read_time, light_status = DAM@data$light_status,
             anova_results = numeric(0)))}

setGeneric("plotStats", function(statsObj) {standardGeneric("plotStats")})
setMethod("plotStats", signature = "DAMstats",
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


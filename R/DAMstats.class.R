# A class that holds several common statistics for a dataset.

setClass("DAMstats", slots = c(attribute = "character", averages = "data.frame",
                               SEM = "data.frame", read_time = "POSIXct",
                               light_status = "integer"))

newStats <- function(DAM = NULL, attributeBy) {
  numFactor <- length(listAttribVals(DAM, attributeBy))
  df <- as.data.frame(matrix(nrow = dim(DAM@data)[1], ncol = numFactor))
  return(new("DAMstats", attribute = attributeBy, averages = df, SEM = df,
             read_time = DAM@data$read_time, light_status = DAM@data$light_status))
}

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
            #TODO add in the read timestamps and light status here
            colnames(plotArray) <- c("index", "attrib", "type", "value")
            plotData <- reshape2::dcast(plotArray, attrib + index ~ type)
            
            # create the actual plot and return it
            gg <- ggplot2::ggplot(plotData, ggplot2::aes(x = index,
                                                         y = AVG,
                                                         ymin = AVG - SEM,
                                                         ymax = AVG + SEM,
                                                         fill = attrib,
                                                         color = attrib)) +
              ggplot2::geom_line() +
              ggplot2::geom_ribbon(alpha = 0.3, color = NA) +
              ggplot2::theme_bw()
            return(gg)
          })

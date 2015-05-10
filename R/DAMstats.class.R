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

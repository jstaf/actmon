#' Synchronize experiment to its light cycle
#' 
#' This method syncs an experiment to a cycling light. This ensures that the
#' start of each experimental day occurs at the same time that the lights switch
#' on or off. Useful for plotting and subsetting your data by days.
#' 
#' @param DAM A valid DAM S4 experiment you wish to sync
#' @param ...
#' @param completeFirstDay A boolean value. If true, the first incomplete day in
#'   the dataset is removed. If false, NAs are added to make an incomplete first
#'   day have 24 full hours. Default: FALSE
#' @param lightFirst A boolean value. If true, the experiment is synced so that 
#'   light periods appear first (otherwise dark periods are first). Default: 
#'   TRUE
#'   
#' @return Returns a DAM S4 object, synced by light cycle.
#' @export
#' 
setGeneric("syncLightCycle", function(DAM, ..., completeFirstDay = FALSE, lightFirst = TRUE) {
  standardGeneric("syncLightCycle")
})
setMethod("syncLightCycle", signature = "DAM",
          definition = function(DAM, ..., completeFirstDay, lightFirst) {
            # First index is where we start cutting things
            tryCatch(first <- getLightChanges(DAM)[1],
                     error = function(e) {
                       stop(paste(e, "Light detection failed."))
                     })
            idxPerHour <- 3600 / getInterval(DAM)
            
            # lag our start point by 12 hours if the first light status != lightFist
            if (DAM@data$light_status[first - 1] != lightFirst) {
              first <- first + idxPerHour * 12 
            }
            
            # add NANs to first day to get it in sync if first day does not need
            # to be complete
            if (!completeFirstDay) {
              # how many NANs do we need to make
              toCreate <- (idxPerHour * 12) - first
              
              # ensure that extra time is still added if toCreate is negative
              if (toCreate < 0) {
                toCreate <- toCreate + (24 * idxPerHour)
              }
              
              if (toCreate != 0) {
                dummy <- as.data.frame(matrix(nrow = toCreate, ncol = dim(DAM@data)[2]))
                colnames(dummy) <- colnames(DAM@data)
                # need to format dummy's dates as posixctlength(DAM@data$read_index) %/% dayLength
                dummy$read_time <- as.POSIXct(dummy$read_time)
                
                DAM@data <- rbind(dummy, DAM@data)
              }
              # we're going to take the whole dataset start now
              first <- 0
            }
            
            hours <- (length(DAM@data$read_index) - first) / idxPerHour
            #TODO make sure we don't accidently lop off a day
            days <- ceiling(hours / 24) * 24
            
            return(subsetTime(DAM, 
                              startTime = first, 
                              expDuration = days, 
                              units = "hours"))
          })

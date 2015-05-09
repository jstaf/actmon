# Determine measurement interval (in seconds)
setGeneric("getInterval", function(obj) {standardGeneric("getInterval")})
setMethod("getInterval", signature = "DAM",
          definition = function(obj) {
            # remove nas
            temp <- na.omit(obj@data)
            idxDiff <- temp[10, 1] - temp[9, 1]
            return(as.numeric(difftime(temp[10, 2], temp[9, 2], units = "secs")) / idxDiff)
          })

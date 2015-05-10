# gets rid of animals that died during an experiment threshold is number of last
# indices that death must have occured before before it is counted
setGeneric("dropDead", function(obj, ..., threshold = 1) {standardGeneric("dropDead")})
setMethod("dropDead", signature = "DAM",
          definition = function(obj, ..., threshold) {
            survival <- calcSurvivalTime(obj)
            toDrop <- names(which(survival < length(obj@data[, 1]) - threshold))
            return(dropAnimals(obj, toDrop))
          })

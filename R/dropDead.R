# gets rid of animals that died during an experiment threshold is number of last
# indices that death must have occured before before it is counted
setGeneric("dropDead", function(obj) {standardGeneric("dropDead")})
setMethod("dropDead", signature = "DAM",
          definition = function(obj) {
            survival <- calcSurvivalTime(obj)
            toDrop <- names(which(!is.na(survival)))
            if (length(toDrop) > 0) {
              print(paste("Vial #", toDrop, "have been detected as dead and will be dropped."))
              return(dropAnimals(obj, toDrop))
            } else {
              print("No animals detected as dead.")
              return(obj)
            }
          })

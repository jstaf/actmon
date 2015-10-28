#' Automatically remove dead animals
#' 
#' This method gets rid of animals that died during an experiment. Death is
#' detected using the \code{\link{calcSurvivalTime}} method.
#' 
#' @seealso \code{\link{calcSurvivalTime}}
#' 
#' @param obj A valid DAM S4 object
#'   
#' @return A DAM S4 object
#' @export
#' 
#' @examples
#' notDead <- dropDead(DAM_DD)
setGeneric("dropDead", function(obj) {standardGeneric("dropDead")})
setMethod("dropDead", signature = "DAM",
          definition = function(obj) {
            survival <- calcSurvivalTime(obj)
            toDrop <- names(which(!is.na(survival)))
            if (length(toDrop) > 0) {
              message(paste("Vial #s", toDrop, "have been detected as dead and will be dropped.", collapse = " "))
              return(dropAnimals(obj, toDrop))
            } else {
              print("No animals detected as dead.")
              return(obj)
            }
          })

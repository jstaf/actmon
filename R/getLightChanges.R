#' Find light changes in a dataset
#' 
#' This is a utility method that returns indices where a Drosophila Activity
#' Monitor detects a change in lighting status.
#' 
#' @param DAM A valid DAM S4 object
#'   
#' @return A vector of indices where light status changed.
#' @export
#' 
setGeneric("getLightChanges", function(DAM) {standardGeneric("getLightChanges")})
setMethod("getLightChanges", signature = "DAM",
          definition = function(DAM) {
            changed <- whichChanged(DAM@data$light_status, 0)
            
            # find difference in indices
            diff <- rep(NA, length(changed) - 1)
            diff[1] <- changed[1]
            for (i in 2:length(diff)) {
              diff[i] <- changed[i + 1] - changed[i]
            }
            
            # Which indices are dramatically shorter than others (probably day/light 
            # cycles that were broken up...)? 
            idx <- which(diff < (mean(diff) / 2))
            if (length(idx) > 1) {
              streaks <- c(1, whichChanged(idx, 1), length(idx))
            } else if (length(idx) == 1) {
              # only occurs if only the start or ending index was less than the
              # mean
              streaks <- idx
            } else {
              stop("No light changes detected.")
            }
            
            # assemble more parsimonious indexes
            if (length(streaks) > 1) {
              for (i in 2:length(streaks)) {
                # Combine streaks of smaller values
                if (i != length(streaks)) {
                  idxInStreak <- streaks[i - 1]:(streaks[i] - 1)
                } else {
                  # need to grab the last index here
                  idxInStreak <- streaks[i - 1]:streaks[i]
                }
                streakSum <- sum(idx[idxInStreak])
                
                # now find which "side" to add the streak sum to
                left <- idx[idxInStreak[1]] - 1
                right <- idx[idxInStreak[length(idxInStreak)]] + 1
                if (left < right) {
                  diff[left] <- diff[left] + streakSum
                } else {
                  diff[right] <- diff[right] + streakSum
                }
              }
              # remove the small value streaks from diff
              return(diff[-idx])
            } else {
              # if there's only one element, its likely at the start/end and we
              # shouldn't remove it
              return(diff)
            }
          })

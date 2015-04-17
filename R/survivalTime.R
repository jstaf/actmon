# Computes the index at which a fly died in the data

survivalTime <- function(vector) {
  zeroCounts <- which(vector < 10)
  if (!is.null(zeroCounts)) {
    lastIdx = rep(NA, length(zeroCounts) - 1)
    # find the time since last zero measurement
    for (i in 1:length(zeroCounts) - 1) {
      lastIdx[i] = zeroCounts[i + 1] - zeroCounts[i]
    }
    #detect if there were multiple periods of no movement
    if (any(lastIdx != 1)) {
      #retrieve the last index+1 where the flies stopped moving for over an hour
      zeroCounts[rev(which(lastIdx != 1))[1] + 1]
    } else {
      # otherwise the first value is the only period of no movement
      zeroCounts[1]
    }
  } else NA
}

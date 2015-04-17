# Changes raw counts to sleep yes/no per 5 min interval.

isAsleep <- function(count) {
  if (count > 0) {
    return(0)
  } else {
    return(100)
  }
}

# Determine measurement interval (in seconds)

getInterval <- function(DAMobject) {
  idxDiff <- DAMobject[2, 1] - DAMobject[1, 1]
  return(as.numeric(difftime(DAMobject[2, 2], DAMobject[1, 2], units = "secs")) / idxDiff)
}

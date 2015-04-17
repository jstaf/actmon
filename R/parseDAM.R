# Parse a standard DAM outputfile

parseDAM <- function(path) {
  # Load file and convert to POSIXct times
  tryCatch({
    DAM <- read.delim(path, as.is = TRUE, header = FALSE)
  }, error = function(err) {
    stop(paste(path, "is not a valid pathname."))
  })
  colnames(DAM) <- c("read_index", "read_date", "read_time", "status", "extra_readings",
                     "unused", "unused", "unused", "unused", "light_status", 1:32)
  DAM$read_time <- as.POSIXct(paste(DAM$read_date, DAM$read_time), format = "%d %b %y %H:%M:%S")
  DAM <- DAM[-which(colnames(DAM) == "read_date")]
  return(DAM)
}

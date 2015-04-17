# Computes standard error of a vector

stdError <- function(vector) {
  sd(vector) / sqrt(length(vector))
}

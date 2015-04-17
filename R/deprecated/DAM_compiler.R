# This script does the same thing as the DAM parser, but compiles and graphs the
# data from multiple "raw sleep counts" output .csv's (from the DAM_parser).

# Read multiple csv files and cbind them together.
inputFiles <- c("",
                "")

# Determine dimensions of matrix used for merging datasets.
mergeDim <- matrix(nrow = length(inputFiles), ncol = 2)
colnames(mergeDim) <- c("numHours", "numFlies")
for (i in 1:length(inputFiles)) {
  file <- read.csv(inputFiles[i], as.is = TRUE, header = TRUE, row.names = 1)
  mergeDim[i,1] <- length(file[,1])
  mergeDim[i,2] <- length(file[1,])
}
numHours <- min(mergeDim[,1]) # Uses the value from whichever dataset has the least timepoints.
numFlies <- sum(mergeDim[,2])

# Okay now iterate through datasets and merge them in turn using the dimensions
# we just calculated.
dataMerge <- data.frame(matrix(nrow = numHours, ncol = numFlies))
lastFileLength <- 0
for (i in 1:length(inputFiles)) {
  file <- read.csv(inputFiles[i],
                   as.is = TRUE, header = TRUE, row.names = 1, check.names = FALSE)
  file <- file[1:numHours,] # slice file to min length defined above
  dataMerge[,(1:length(file[1,]) + lastFileLength)] <- file
  colnames(dataMerge)[(1:length(file[1,]) + lastFileLength)] <- colnames(file)
  lastFileLength <- length(file[1,]) + lastFileLength
}

# iterate through genotypes and calculate average amount of sleep for each
genotypes <- levels(as.factor(colnames(dataMerge)))
genotypes <- genotypes[which(genotypes != "elav>Dat_GD_0day")]
hourAvgSleep <- matrix(nrow = numHours, ncol = length(genotypes))
for (i in c(1:length(genotypes)) ) {
  level <- as.character(genotypes[i])
  genotype_sub <- dataMerge[which(colnames(dataMerge) == level)]
  hourAvgSleep[,i] <- rowMeans(genotype_sub)
}
colnames(hourAvgSleep) <- as.character(genotypes)
rownames(hourAvgSleep) <- c(1:numHours)

# plot ALL the things!
library(reshape2)
library(ggplot2)
plotData <- melt(hourAvgSleep, by = colnames(hourAvgSleep))

DAMplot <- ggplot(plotData, aes(x = Var1, y = value, color = Var2))
DAMplot <- DAMplot + geom_line()
DAMplot <- DAMplot + xlab("Time since experiment start (hours)") +
  ylab ("Percent of time asleep per hour") +
  scale_x_continuous(breaks=(1:numHours%/%12)*12) +
  scale_y_continuous(breaks=(0:4)*25 )
DAMplot

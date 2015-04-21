# This script is designed to parse DAM output files and create intelligible output.
# DAM <- read.delim(file.choose(), as.is = TRUE, header = FALSE)
# sample_info <- read.csv(file.choose())
DAM <- read.delim("data/exampleDAM.txt", as.is = TRUE, header = FALSE)
sample_info <- read.csv("data/example_smpinfo.csv")

titleName = ""

# any columns you wish to exlcude (like if a fly died halfway through)? Just
# enter the number of which vials you want to exclude in a vector here. Leave
# the vector empty if there's nothing you want to exclude.
excluded <- c()

# any times you wish to exlclude? Enter the number of hours to chop off from
# start and exoeriment duration.
hours_start <- c()
exp_duration <- c()

# enter values from other factors column that you wish to INCLUDE, otherwise
# leave blank if you want everything.
other_factor <- c()

colnames(DAM) <- c("read_index", "read_date", "read_time", "status", "extra_readings",
                   "unused", "unused", "unused", "unused", "light_status", 1:32)
# Use only data points where DAM reports a good data read (aka status == 1).
#DAM <- subset(DAM, DAM$status == 1)
counts <- DAM[,11:42]
timeLabels <- DAM[,1:3]

# Subset out different datasets with different values in "other_factors" column.
# Change level to a vector with the values you want to include, use
# levels(sample_info$other_factors) for all.
sample_info$other_factors <- as.factor(sample_info$other_factors)
if (is.null(other_factor) == FALSE) {
  level <- other_factor
} else {
  level <- levels(sample_info$other_factors)
}
info_level <- subset(sample_info, other_factors %in% level)
info_level_idx <- match(info_level$vial_number, colnames(counts))

# Subset out only the hours you want to analyse.
if (is.null(hours_start) == FALSE) {
  hours_start  <- hours_start*12
} else {
  hours_start <- 0
}
if (is.null(exp_duration) == FALSE) {
  exp_duration <- exp_duration*12
} else {
  exp_duration <- length(counts[,1]) - hours_start
}

# Okay subset out the data we want.
counts_dropped <- counts[hours_start:(hours_start + exp_duration),info_level_idx]

# Changes raw counts to sleep yes/no per 5 min interval.
sleep <- function(count) {
  if (count > 0) {
    return(0)
  } else {
    return(100)
  }
}
#countsMatrix <- as.matrix(counts_dropped)
countsMatrix <- apply(counts_dropped, c(1,2), sleep)

# Now compute percent sleep per hour.
hours <- length(countsMatrix[,1])%/%12
remainder <- length(countsMatrix[,1])%%12
hourCounts <- colMeans(matrix(
  countsMatrix[1:(length(countsMatrix[,1])-remainder),],
  nrow = 12))
hourCounts <- matrix(hourCounts ,nrow = hours, ncol = ncol(countsMatrix))
colnames(hourCounts) <- colnames(countsMatrix)

survivalTime <- function(vector) {
  zeroCounts <- which(vector < 10)
  if (!is.null(zeroCounts)) {
    lastIdx = rep(NA, length(zeroCounts)-1)
    # find the time since last zero measurement
    for (i in 1:length(zeroCounts)-1) {
      lastIdx[i] = zeroCounts[i+1]- zeroCounts[i]
    }
    #detect if there were multiple periods of no movement
    if (any(lastIdx != 1)) {
      #retrieve the last index+1 where the flies stopped moving for over an hour
      zeroCounts[rev(which(lastIdx != 1))[1]+1]
    } else {
      # otherwise the first value is the only period of no movement
      zeroCounts[1]
    }
  } else NA
}
survival <- apply(hourCounts, 2, survivalTime)

stdError <- function(vector) {
  sd(vector)/sqrt(length(vector))
}

# Iterate through genotypes and calculate average amount of line breaks/survival for each
hourAvg <- matrix(nrow = hours, ncol = length(unique(sample_info$genotype)))
hourSEM <- matrix(nrow = hours, ncol = length(unique(sample_info$genotype)))
survivalAvg <-rep(NA, length(unique(sample_info$genotype)))
survivalSEM <- rep(NA, length(unique(sample_info$genotype)))

genotypes <- unique(info_level$genotype)
if (is.null(excluded) == FALSE) {
  genotype_level <- info_level[-excluded,]
} else {
  genotype_level <- info_level
}

for (i in c(1:length(genotypes)) ) {
  level <- as.character(genotypes[i])
  # Subset out each genotype.
  levelToTest <- subset(genotype_level, genotype_level$genotype == level)
  genotype_idx <- match(levelToTest$vial_number, colnames(hourCounts))
  # Compute average counts for that genotype for any given hour.
  hourAvg[,i] <- rowMeans(hourCounts[,genotype_idx])
  hourSEM[,i] <- apply(hourCounts[,genotype_idx], 1, stdError)

  survivalAvg[i] <- mean(survival[genotype_idx])
  survivalSEM[i] <- stdError(survival[genotype_idx])
}
names(survivalAvg) <- as.character(genotypes)
names(survivalSEM) <- as.character(genotypes)

## plot the count data
# cat together and reshape all the data
library(reshape2)
countArray <- array(data = c(hourAvg, hourAvg-hourSEM, hourAvg+hourSEM), dim = c(dim(hourAvg),3))
countArray[countArray < 0] <- 0
dimnames(countArray) <- list(c(1:hours), as.character(genotypes), c("AVG","minusSEM", "plusSEM"))
meltArray <- melt(countArray)
plotData <- dcast(meltArray, Var2 + Var1 ~ Var3)
colnames(plotData)[1] <- "Genotype"

# Graph output
library(ggplot2)
DAMplot <- ggplot(plotData, aes(x = Var1, y = AVG, ymin = minusSEM, ymax = plusSEM, fill = Genotype, color = Genotype))
DAMplot <- DAMplot + geom_line() + geom_ribbon(alpha = 0.3, color = NA) +
  xlab("Time since experiment start (hours)") +
  ylab ("Activity (beam breaks/hour)") + ggtitle(titleName) +
  scale_x_continuous(breaks=(1:hours%/%12)*12)

#add a bunch of doodads
background_fill <- "white"
DAMplot <- DAMplot + theme(panel.background = element_rect(fill = background_fill),
                panel.grid.major = element_line(colour = background_fill),
                panel.grid.minor = element_line(colour = background_fill)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
DAMplot

# plot the survival data
# reshape and reorder
survMatrix <- data.frame(as.character(genotypes), survivalAvg, survivalAvg-survivalSEM, survivalAvg+survivalSEM)
colnames(survMatrix) <- c("Genotype", "AVG", "minusSEM", "plusSEM")
survMatrix$Genotype <- factor(survMatrix$Genotype, levels = survMatrix$Genotype)
survivalPlot <- ggplot(survMatrix, aes(x = Genotype, fill = Genotype,
                                       y = AVG, ymin = minusSEM, ymax = plusSEM)) +
  geom_bar(stat = "identity", width = 0.8) +
  geom_errorbar(width = 0.4) +
  guides(fill = FALSE) + ggtitle(titleName) +
  xlab(NULL) + ylab("Survival time (hours)") +
  theme(panel.background = element_rect(fill = background_fill),
        panel.grid.major = element_line(colour = background_fill),
        panel.grid.minor = element_line(colour = background_fill)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
survivalPlot

# # Write output
# label_idx <- match(colnames(counts_dropped), sample_info$vial_number)
# colnames(hourCounts) <- as.character(sample_info$genotype)[label_idx] # label by genotype
# rownames(hourCounts) <- c(1:hours)
# if (is.null(excluded) == FALSE) {
#   hourCounts <- hourCounts[,-excluded]
# }
# write.csv(hourCounts, file = raw_outputFile)

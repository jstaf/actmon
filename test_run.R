# This script tests out the functions so far
library(actMon)

# Load data and create an experiment object
dat <- parseDAM("data/exampleDAM.txt")
info <- read.csv("data/example_smpinfo.csv")
exp <- newExperiment(dataFile = dat, infoFile = info)
exp <- calcSleep(exp)
exp <- toInterval(exp, 1, units = "hours", aggregateBy = "average")
#listAttributes(exp)
#listAttribVals(exp, "genotype")
stats <- calcStats(exp, "genotype")
plotStats(stats)

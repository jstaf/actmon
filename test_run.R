# This script tests out the functions so far
library(actMon)

# Load data and create an experiment object
exp <- newExperiment(dataFile = "data/exampleDAM.txt", infoFile = "data/example_smpinfo.csv")
exp <- calcSleep(exp)
exp <- toInterval(exp, 1, units = "hours", aggregateBy = "average")
listAttributes(exp)
listAttribVals(exp, "sex")
exp <- byAttribute(exp, "0", "sex")
stats <- calcStats(exp, "genotype")
plotStats(stats)

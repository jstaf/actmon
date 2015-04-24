# This is a list of functions included in this package and their usage. Not the
# greatest documentation, but keep in mind this is a work in progress...

# Create a new experiment object from your activity monitor data and its 
# accompanying info file. This is generally what all of the other functions in 
# this package operate on (for the rest of this documentation, obj represents
# one of these objects).
newExperiment(dataFile = "pathToData", infoFile = "pathToInfoFile")
# If this doesn't work, make sure the files you are trying to access are in your
# working directory (getwd() and setwd() are handy!).

# Show the possible "attributes", or column names in your file. These can be
# named anything. An example of an attribute woud be  genotype.
listAttributes(obj)

# Show the all of the possible values for an attribute.
listAttribVals(obj, "attributeName")

# Select data that matches the values for attributes you list. Values that don't
# match the values you specify will be removed. The values to keep can be a vector.
byAttribute(obj, "valuesToKeep", "attributeName")

# Remove the data that matches the attributes you specify. Values that DO MATCH
# get removed. The values to remove can be a vector. 
dropAttribute(obj, "valuesToRemove", "attributeName")

# Combine a vector of experiments into a single experiment.
catExperiments(c(obj1, obj2, etc))

# Take a specific period of time and remove everything else. Specify the amount
# of time since the start of the data file, and the duration of time of the
# experiment to include after that. Specify the units of time as well.
subsetTime(obj, startTime, experimentDuration, units)
# units can be one of c("seconds","minutes","hours")

# Calculate the amount of sleep for each fly. Generally needs the interval
# between readings to be 5 minutes.
calcSleep(obj)

# Reduce the time interval between timepoints to whatever you specify. Data is
# compressed either by summing or averaging all of the values for each timepoint.
toInterval(obj, amtOfTime, units = "hours", aggregateBy = "average")
# units can be one of c("seconds", "minutes", "hours")
# aggregateBy can be one of c("average", "sum")

# Calculate stats (just average and SEM for now, more later...)
calcStats(obj)

# Plot the output of calcStats()
plotStats(stats)

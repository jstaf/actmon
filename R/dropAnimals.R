# drops columns of data
setGeneric("dropAnimals", function(obj, vialNumbers) {standardGeneric("dropAnimals")})
setMethod("dropAnimals", signature = "DAM", 
          definition = function(obj, vialNumbers) {
            vialNumbers <- as.character(vialNumbers)
            
            # remove vials from sample info
            idx <- which(obj@sample_info[, 1] %in% vialNumbers)
            obj@sample_info <- obj@sample_info[-idx, ]
            
            # remove vials from datasheet
            lightsCol <- which(colnames(obj@data) == "light_status")
            offsetIdx <- lightsCol + idx
            obj@data <- obj@data[, -offsetIdx]
            
            return(obj)
          })

### Generate global gridded temperature profile 
#### source
source("prepare.R")


#### read in data
inName <- "data/ecmwf_sst_1979.nc"

##Project onto right resolution and coordinates
d <- brick(inName, varname = "sst")
outDF <- rasterToPoints(d)
outDF <- as.data.frame(outDF, stringsAsFactors=F)

colnames(outDF) <- c("x", "y", "Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec")

##Save data as excel file
outName <- inName
outName <- sub("*\\.nc", ".csv", outName)
outName <- sub("data/", "output/", outName)

write.csv(outDF, outName,
          row.names=F) 


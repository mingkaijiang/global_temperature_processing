get_lonlatDF <- function(sourceDir, destDir) {
  #### read in data
  inName <- paste0(sourceDir, "era_interim_2m_temperature_6_hourly_1979_apr.nc") 
  
  ### open nc file
  nc <- nc_open(inName)
  
  ### get the variables
  lon <- ncvar_get(nc, "longitude")
  lat <- ncvar_get(nc, "latitude")
  
  ### create a lonlat file
  lonlat <- as.data.frame(as.matrix(expand.grid(lon,lat)))
  colnames(lonlat) <- c("lon", "lat")
  
  write.csv(lonlat, paste0(destDir, "/lonlat.csv"), row.names=F)
}
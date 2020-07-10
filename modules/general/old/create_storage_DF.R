create_storage_DF <- function(sourceDir) {
    
    #### read in data
    inName <- paste0(sourceDir,"era_interim_2m_temperature_6_hourly_1979_jan.nc") 
    
    ### open nc file
    nc <- nc_open(inName)
    
    ### get the variables
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")
    
    
    lonlat <- as.data.frame(as.matrix(expand.grid(lon,lat)))
    colnames(lonlat) <- c("lon", "lat")
    
    nc_close(nc)
    
    return(lonlat)
}
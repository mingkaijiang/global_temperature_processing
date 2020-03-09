cru_data_check <- function() {
    
    ### data source:
    ### https://crudata.uea.ac.uk/cru/data/temperature/
    ### absolute temperature  for the base period 1961-90 
    ### on a 5° by 5° grid (Jones et al., 1999). 
    ### Note that in this file, latitudes run from North to South.
    
    
    
    #### read in data
    inName <- paste0("data/cru_absolute.nc") 
    
    ### open nc file
    nc <- nc_open(inName)
    
    ### get the variables
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    time <- ncvar_get(nc, "time") # hours since 1900-01-01 00:00:00.0
    
    
    ### create lonlat data frame
    lonlat <- as.data.frame(as.matrix(expand.grid(lon,lat)))
    colnames(lonlat) <- c("lon", "lat")
    
    ### prepare time series
    dnameDF <- data.frame(c("jan", "feb", "mar", "apr", "may", "jun",
                                "jul", "aug", "sep", "oct", "nov", "dec"))
    colnames(dnameDF) <- c("month")

    dname <- c("jan", "feb", "mar", "apr", "may", "jun",
               "jul", "aug", "sep", "oct", "nov", "dec")
    
    ### calculate monthly temperature mean, of 1961-1990
    
    ### read in the 3d file
    tmp_array <- ncvar_get(nc,"tem")
    
    ### create a lonlat file
    lonlat <- as.matrix(expand.grid(lon,lat))
    
    ### use the first date and time to create a storage df
    tmp_slice <- tmp_array[,,1]
    tmp_vec <- as.vector(tmp_slice)
    
    ### make a plot to visually inspect the result
    ### note that the latitude is not increasing, so need to reverse 
    # image(lon,rev(lat),tmp_slice, col=rev(brewer.pal(10,"RdBu")))
    
    ### create the storage file
    tmpDF <- data.frame(cbind(lonlat,tmp_vec), NA, NA, NA, NA, NA, NA,
                        NA, NA, NA, NA, NA)
    names(tmpDF) <- c("lon","lat",dname)
    
    
    ### get a slice
    for (i in 2: 12) {
        tmp_slice <- tmp_array[,,i]
        tmp_vec <- as.vector(tmp_slice)
        tmpDF[,(i+2)] <- tmp_vec
    }
    
    ### close the nc
    nc_close(nc)
    
    ### now we obtain base means for 1961-1990, for each month of a year
    ### we need to add anomaly to these values, to caculate real absolute values for each grid, month and year
    ### then we can compute interannual mean, sd, Tgrowth and Topt
    ### then we can calculate the parameter to see if we have a universal number
    
}
prepare_monthly_output <- function(dname) {
    
    #### read in data
    inName <- paste0("data/era_interim_2m_temperature_6_hourly_", dname, ".nc") 
    
    ### open nc file
    nc <- nc_open(inName)
    
    ### get the variables
    time <- ncvar_get(nc, "time") # hours since 1900-01-01 00:00:00.0
    lon <- ncvar_get(nc, "longitude")
    lat <- ncvar_get(nc, "latitude")
    
    ### get length
    nlon <- length(lon)
    nlat <- length(lat)
    ntime <- length(time)
    
    ### read in the 3d file
    tmp_array <- ncvar_get(nc,"t2m")
    
    ### create a lonlat file
    lonlat <- as.matrix(expand.grid(lon,lat))
    
    ### use the first date and time to create a storage df
    tmp_slice <- tmp_array[,,1]
    tmp_vec <- as.vector(tmp_slice)
    
    ### make a plot to visually inspect the result
    ### note that the latitude is not increasing, so need to reverse 
    # image(lon,rev(lat),tmp_slice, col=rev(brewer.pal(10,"RdBu")))
    
    ### create the storage file
    tmpDF <- data.frame(cbind(lonlat,tmp_vec))
    names(tmpDF) <- c("lon","lat",paste(dname,as.character(time[1]), sep="_"))
    
    
    ### get a slice
    for (i in 2: ntime) {
        tmp_slice <- tmp_array[,,i]
        tmp_vec <- as.vector(tmp_slice)
        tmpDF[,(i+2)] <- tmp_vec
    }
    
    ### close the nc
    nc_close(nc)
    
    
    ### now summarize mean, sd, and sample size for each grid
    tmp.matrix <- as.matrix(tmpDF[,-c(1:2)])
    grid.mean <- rowMeans(tmp.matrix, na.rm=T)
    grid.sd <- rowSds(tmp.matrix, na.rm=T)
    grid.n <- ntime
    
    outDF <- data.frame(grid.mean, grid.sd, grid.n)
    colnames(outDF) <- c(paste0("m_", dname),
                         paste0("s_", dname),
                         paste0("n_", dname))
    
    return(outDF)
    
}
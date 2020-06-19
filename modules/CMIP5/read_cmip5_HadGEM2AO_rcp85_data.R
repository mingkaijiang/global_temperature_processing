read_cmip5_HadGEM2AO_rcp85_data <- function() {
    
    
    ### open nc file
    nc <- nc_open("data/clt_day_CMCC-CM_rcp85_r1i1p1_20060101-20061231.nc")
    
    ### get the variables
    time <- ncvar_get(nc, "time") # day of year
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    ### get length
    nlon <- length(lon)
    nlat <- length(lat)
    ntime <- length(time)
    
    ## time id
    time.id <- c(1:ntime)
    
    ### read in the 3d file
    tmp_array <- ncvar_get(nc,"clt")
    
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
    names(tmpDF) <- c("lon","lat",paste("DOY",as.character(time.id[1]), sep="_"))
    
    
    ### get a slice
    for (i in 2: ntime) {
        tmp_slice <- tmp_array[,,i]
        tmp_vec <- as.vector(tmp_slice)
        tmpDF[,(i+2)] <- tmp_vec
    }
    
    ### close the nc
    nc_close(nc)
    
    ### test plotting
    plot(raster(tmp_slice))
    
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
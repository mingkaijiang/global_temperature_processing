read_cmip5_HadGEM2AO_rcp85_data <- function() {
    
    
    ### open nc file
    nc <- nc_open("data/tas_day_HadGEM2-AO_rcp85_r1i1p1_20060101-21001230.nc")
    
    ### get the variables
    time <- ncvar_get(nc, "time") # day since 2006-01-01
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    ### get length
    nlon <- length(lon)
    nlat <- length(lat)
    ntime <- length(time)
    
    ## time id
    time.id <- c(1:ntime)
    
    ## date DF
    dateDF <- data.frame(time.id, NA, NA, NA, NA)
    colnames(dateDF) <- c("nday", "year", "month", "date", "yearmonth")
    dateDF$date <- as.Date("2015-12-31") + dateDF$nday
    dateDF$yearmonth <- substr(dateDF$date, 1, 7)
    dateDF$year <- as.numeric(substr(dateDF$date, 1, 4))
    dateDF$month <- as.numeric(substr(dateDF$date, 6, 7))
    
    ### index ID
    yearmonth <- unique(dateDF$yearmonth)
    group.id <- c(1:length(yearmonth))
    idDF <- data.frame(yearmonth, group.id)
    dateDF <- merge(dateDF, idDF, by="yearmonth")

    ### read in the 3d file
    tmp_array <- ncvar_get(nc,"tas")
    
    ### close the nc
    nc_close(nc)
    
    ### create a lonlat file
    lonlat <- as.data.frame(expand.grid(lon,lat))
    colnames(lonlat) <- c("lon", "lat")
    
    ### create storage DF
    tmpDF <- array(NA, c(nlon, nlat, length(group.id)))
    tmpDF.sd <- tmpDF.n <- tmpDF
    
    ### assign value
    for (i in group.id) {
        ## get location information
        loc.s <- min(dateDF$nday[dateDF$group.id == i])
        loc.e <- max(dateDF$nday[dateDF$group.id == i])
        ndays <- loc.e - loc.s + 1
        
        ## subset data based on date range
        tmp_slice <- tmp_array[,,loc.s:loc.e]
        
        ## calculate mean and sd of the matrix data
        tmpDF[,,i] <- rowMeans(tmp_slice, dim = 2)
        tmpDF.sd[,,i] <- apply(tmp_slice, c(1,2), sd)
        tmpDF.n[,,i] <- ndays
    }
    
    ### save data
    saveRDS(lonlat, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_lonlat.rds"))
    saveRDS(tmpDF, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_Tmean.rds"))
    saveRDS(tmpDF.sd, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_Tsd.rds"))
    saveRDS(tmpDF.n, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_n.rds"))
    
}
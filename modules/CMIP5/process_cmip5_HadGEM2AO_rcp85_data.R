process_cmip5_HadGEM2AO_rcp85_data <- function(sourceDir, destDir) {
    
    
    ### open nc file
    nc <- nc_open(paste0(sourceDir, "/tas_day_HadGEM2-AO_rcp85_r1i1p1_20060101-21001230.nc"))
    
    ### get the variables
    time <- ncvar_get(nc, "time") # day since 2006-01-01
    lon <- ncvar_get(nc, "lon")
    lat <- ncvar_get(nc, "lat")
    
    ### get length
    nlon <- length(lon)
    nlat <- length(lat)
    ntime <- length(time) # 95 years, each year has 360 days
    time.id <- c(1:ntime)
    yr.list <- c(2006:2100)

    ## date DF
    dateDF <- data.frame(rep(yr.list, each = 360), 
                         rep(c(1:12), each = 30), 
                         rep(c(1:30), 1140), 
                         NA)
    colnames(dateDF) <- c("year", "month", "dom", "yearmonth")
    dateDF$yearmonth <- paste0(dateDF$year, "-", dateDF$month)
    dateDF$nday <- time.id
    
    ## subset period 2060 to 2099, 40 years
    dateDF <- subset(dateDF, year >= 2060 & year <= 2099)
    dateDF$group <- rep(c(1:480), each = 30)
    
    ## generate group id
    group.id <- unique(dateDF$group)
    
    ## read in the CMIP data
    tmp_array <- ncvar_get(nc,"tas")
    
    ### close the nc
    nc_close(nc)
    
    ### create a lonlat file
    lonlat <- as.data.frame(expand.grid(lon,lat))
    colnames(lonlat) <- c("lon", "lat")
    
    ### create storage DF
    tmpDF <- array(NA, c(nlon, nlat, length(group.id)))
    tmpDF.sd <- tmpDF
    
    ### assign value
    for (i in group.id) {
        ## get location information
        loc.s <- min(dateDF$nday[dateDF$group == i])
        loc.e <- loc.s + 30 - 1
        
        ## subset data based on date range
        tmp_slice <- tmp_array[,,loc.s:loc.e]
        
        ## calculate mean and sd of the matrix data
        tmpDF[,,i] <- rowMeans(tmp_slice, dim = 2)
        tmpDF.sd[,,i] <- apply(tmp_slice, c(1,2), sd)
    }
    
    ### sample size is always n = 30
    lonlat$n <- 30
    
    ### save data
    saveRDS(lonlat, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_lonlat.rds"))
    saveRDS(tmpDF, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_Tmean.rds"))
    saveRDS(tmpDF.sd, paste0(destDir, "/cmip5_rcp85_HadGEM2AO_Tsd.rds"))

}
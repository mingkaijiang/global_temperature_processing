prepare_inter_annual_output_rcp85_HadGEM2AO <- function(sourceDir, destDir, 
                                                        return.option) {
    
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ### read in the data
    inDF <- readRDS(paste0(sourceDir, "/cmip5_rcp85_HadGEM2AO_Tmean.rds"))
    lonlatDF <- readRDS(paste0(sourceDir, "/cmip5_rcp85_HadGEM2AO_lonlat.rds"))
    
    ### get dimension information
    nlon <- dim(inDF)[1]
    nlat <- dim(inDF)[2]
    
    ## convert temperature from K to C
    inDF <- inDF - 273.15
    
    ### number of years
    yr.list <- c(2060:2099)
    n.yr <- 40
    
    ## prepare annual DF to store the annual mean data
    annDF <- array(NA, c(nlon, nlat, 40))
    
    ### now we have two return options:
    ### 1. calculate grow season temperature, i.e., mean temperature of months when T > 0 
    ### 2. calculate annual mean temperature
    if (return.option == "growth") {
        ### calculate growth temperature as months when T > 0
        
        ### convert month when T < 0 to NA
        inDF[inDF < 0.0] <- NA
        
        ### subset each year
        for (i in 1:n.yr) {
            loc.s <- (i-1) * 12 + 1
            loc.e <- 12 * i
            
            ## subset data based on date range
            tmp_slice <- inDF[,,loc.s:loc.e]
            
            ## calculate mean and sd of the matrix data
            annDF[,,i] <- rowMeans(tmp_slice, dim = 2, na.rm=T)
        }
        
        ### create storage DF
        outDF <- array(NA, c(nlon, nlat, 4))
        
        ### calculate inter-annual Tgrowth, Tsd, Topt, Tparam
        outDF[,,1] <- rowMeans(annDF, dim = 2, na.rm=T)
        outDF[,,2] <- apply(annDF, c(1,2), sd, na.rm=T)
        outDF[,,3] <- 13.9 + 0.61 * outDF[,,1] 
        
        outDF[,,4] <- (outDF[,,3] - outDF[,,1]) / outDF[,,2]
        
        ### melt 
        lonlatDF$T_mean <- melt(outDF[,,1])$value
        lonlatDF$T_sd <- melt(outDF[,,2])$value
        lonlatDF$T_opt <- melt(outDF[,,3])$value
        lonlatDF$T_param <- melt(outDF[,,4])$value
        
        ### prepare sea surface area mask
        ssfDF <- read_sea_surface_mask()
        ssfDF$ssf <- ifelse(is.na(ssfDF$ssf), 2, 1) # 2 is land
        coordinates(ssfDF)=~lon+lat
        gridded(ssfDF) <- T
        ssf.r <- raster(ssfDF)
        
        ### extract ssf based on CMIP data grid information
        lonlat <- lonlatDF[,c("lon", "lat")]
        names(lonlat) <- c("x", "y")
        
        ssfDF.rev <- cbind(extract(ssf.r, lonlat, df = T), lonlat)
        colnames(ssfDF.rev) <- c("ID", "ssf", "lon", "lat")
        
        ### merge ssf and TgrDF
        mgDF <- merge(lonlatDF, ssfDF.rev, by=c("lon", "lat"))
        
        ### subtract only land
        landDF <- mgDF[mgDF$ssf == 2,]
        
        landDF$ID <- NULL
        
        
        ### write csv
        write.csv(landDF, paste0(destDir, "/Tmean_grow_season.csv"),
                  row.names=F)
        
        
    } else if (return.option == "annual") {
        ### return annual mean T for each year
        
        print("no script prepared for annual")
        
    }
    
    
 
    ### return
    return(landDF)
}
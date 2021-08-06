get_elevation_data <- function(plotDF,sd.filter.option,
                               outdir, outname) {
    
    
    #require(elevatr)
    
    ### remove NAs
    plotDF <- plotDF[!is.na(plotDF$T_sd),]
    
    ### delete antarctica
    plotDF <- plotDF[plotDF$lat > -62, ]
    
    ### convert lon to make nicer wide plot
    plotDF$lon2 <- ifelse(plotDF$lon >180, (plotDF$lon - 360), plotDF$lon)
    
    
    ### filter
    if (sd.filter.option == "no.filter") {
        print("no filter")
        plotDF <- plotDF
    } else if (sd.filter.option == "filter") {
        print("filter by replacing Tsd < 0.5 with 0.5")
        ### delete unreasonably small T sd
        plotDF <- plotDF[plotDF$T_sd >= 0.5, ]
        #plotDF$T_sd <- ifelse(plotDF$T_sd >= 0.5, plotDF$T_sd, 0.5)
        
        plotDF$T_param <- with(plotDF, T_opt - T_mean) / plotDF$T_sd
        
    }

    corDF <- plotDF[,c("lon2", "lat")]
    colnames(corDF) <- c("x","y")
    
    
    ### read tif at 10 km resolution
    ### data source: https://www.earthenv.org/topography
    require(tiff)
    require(raster)
    demDF <- readTIFF(paste0("data/elevation_10KMmd_GMTEDmd.tif"))
    
    
    
    
    
    
    #outDF <- corDF
    #outDF$elev <- NA
    #
    #id.list <- c(1,501,1001,1501,2001,2251,2501,2751,3001,3501,4001,4501,
    #             5001,5501,6001,6501,7001,7501,8001,8501,
    #             9001,9501,10001,10501,11001,11501,12001,12501,
    #             13001,13501,14001,14501,15001,15501,
    #             16001,16501,17001,17501,18001,18501,
    #             19001,19501,20001,20501,21001,21501,22001,22501,
    #             23001,23501,24001,24501,25001,25501,
    #             26001,26501,27042)
    #
    #l <- length(id.list)
    #
    #for (i in 1:(l-1)){
    #    subDF <- corDF[id.list[i]:(id.list[i+1]-1),]
    #    
    #    ll_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    #    #ll_prj <- "+init=EPSG:4326"
    #    
    #    cor.points <- sp::SpatialPoints(sp::coordinates(subDF),
    #                                    proj4string = CRS(ll_prj))
    #    
    #    
    #    test <- get_elev_point(locations=cor.points,
    #                           prj=ll_prj,
    #                           src = c("aws"))
    #    
    #    outDF$elev[id.list[i]:(id.list[i+1]-1)] <- test$elevation
    #    
    #}
    
    
    
    
    
    
   ### end    
}
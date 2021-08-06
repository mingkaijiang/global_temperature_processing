get_elevation_data <- function(plotDF,sd.filter.option,
                               outdir, outname) {
    
    ### create out directory
    if(!dir.exists(outdir)) {
        dir.create(outdir, showWarnings = FALSE)
    }
    
    ### read tif at 10 km resolution
    ### data source: https://www.earthenv.org/topography
    require(tiff)
    require(raster)
    demDF <- readTIFF(paste0("data/elevation_10KMmd_GMTEDmd.tif"))
    
    r1 <- raster(demDF)
    plot(r1)
    #res(r)
    #extent(r)
    
    bb <- extent(-180, 180, -56, 84)
    extent(r1) <- bb
    r1 <- setExtent(r1, bb, keepres=F)
    
    r2 <- raster(nrow=720, ncol=1440,
                 xmn=-180, xmx=179.75,
                 ymn=-90, ymx=89.75)
    
    rs <- resample(r1, r2, method="bilinear")
    
    #test <- aggregate(r1, fact=2, fun=mean, expand=T, na.rm=T)
    #rs2 <- resample(test, r2, method="bilinear")
    
    #plot(rs2)
    
    ### convert into xy
    demDF <- as.data.frame(rs, xy=T)
    
    lon <- seq(-180, 179.75, by=0.25)
    lat <- seq(-90, 89.75, by=0.25)
    
    demDF$lon <- rep(lon, length(lat))
    demDF$lat <- rep(rev(lat), each=length(lon))
    
    
    #p3 <- ggplot() + 
    #    geom_tile(data=demDF, aes(y=lat, x=lon, fill=layer)) +
    #    borders(col=alpha("black", 0.8), lwd=0.1)+
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=8),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"),
    #          legend.position = c(0.1, 0.35),
    #          panel.background=element_rect(fill="white", colour="black"),
    #          legend.background = element_rect(fill="grey",
    #                                           size=0.5, linetype="solid", 
    #                                           colour ="black"))+
    #    scale_x_continuous(name ="Longitude",
    #                       breaks=c(-180, -90, 0, 90, 180),
    #                       limits=c(-180, 180))+
    #    scale_y_continuous(name ="Latitude", 
    #                       breaks=c(-90, -45, 0, 45, 90),
    #                       limits=c(-90, 90)); p3
    #
    
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

    
    outDF <- merge(plotDF, demDF, by.x=c("lon2", "lat"),
                   by.y=c("lon","lat"),
                   all.x=T)
    
    names(outDF)[names(outDF)=="layer"] <- "elevation"
    
    summary(outDF$elevation)
    
    
    p3 <- ggplot() + 
        geom_tile(data=outDF, aes(y=lat, x=lon2, fill=elevation)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        #scale_fill_manual(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
        #                  values = col_discrete,
        #                  labels = Tparam_lab)+
        borders(col=alpha("black", 0.8), lwd=0.1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position = c(0.1, 0.35),
              panel.background=element_rect(fill="white", colour="black"),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180),
                           limits=c(-180, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-90, -45, 0, 45, 90),
                           limits=c(-90, 90))+
        guides(fill = guide_legend(ncol = 1, byrow = TRUE)); p3
    
    pdf(paste0(outdir, outname, "_dem_map_plot.pdf"),
        width=10, height=6)
    plot(p3)
    dev.off()
    
    
    ### linear fit
    fit1 <- lm(T_param~elevation, data=outDF)
    
    ### plot 
    p1 <- ggplot(outDF, aes(x=elevation, y = T_param)) + 
        geom_hex(bins = 10, color = "black") +
        geom_point(outDF, mapping=aes(x=elevation, y = T_param), col=alpha("black",0.2))+
        geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], col="red", lwd=2)+
        #scale_fill_continuous(name = "Grid density", type = "viridis") +
        scale_fill_stepsn(name = "Grid density", colors = rev(topo.colors(10))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = c(0.85, 0.85),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression("Elevation (m)"))+
        scale_y_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd])); p1
    
    
    pdf(paste0(outdir, outname, "_dem_correlation_plot.pdf"),
        width=6, height=6)
    plot(p1)
    dev.off()
    

    #return(outDF)
   ### end    
}
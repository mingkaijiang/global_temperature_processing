merge_biome_information <- function(plotDF, sd.filter.option,
                                    outdir, outname) {
    
    biomeDF <- read_in_biome_information(plot.option = T)
    
    
    
    ########################### some modification to the dataset ###########################
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
        print("filter by replacing Tsd < 1 with 1")
        ### delete unreasonably small T sd
        #plotDF <- plotDF[plotDF$T_sd >= 1.0, ]
        plotDF$T_sd <- ifelse(plotDF$T_sd >= 1.0, plotDF$T_sd, 1.0)
    }
    
    
    ### prepare cos(latitude) to weight the T value 
    plotDF$T_mean_weighted <- plotDF$T_mean / cos(plotDF$lat)
    plotDF$T_sd_weighted <- plotDF$T_sd / cos(plotDF$lat)
    plotDF$T_opt_weighted <- plotDF$T_opt / cos(plotDF$lat)
    plotDF$T_param_weighted <- plotDF$T_param / cos(plotDF$lat)
    
    
}

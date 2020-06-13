merge_biome_information_and_plot <- function(plotDF, sd.filter.option,
                                             outdir, outname) {
    
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
    
    
    
    ########################### overlay biome grids to plot grids ###########################
    ### overlay biome grids with plot grids
    DF1 <- plotDF[,c("lon2", "lat")]
    colnames(DF1) <- c("lon", "lat")
    DF2 <- read_in_biome_information(plot.option = T)
    
    ### convert from xy to raster
    coordinates(DF2) <- ~lon+lat
    gridded(DF2) <- T
    r2 <- raster(DF2)
    
    ### extract BIOME categorization according to xy information of plotDF
    biomeDF <- cbind(DF1, extract(r2, DF1, df=T))
    
    
    ### assign biome information to plotDF
    plotDF.rev <- merge(plotDF, biomeDF, by.x=c("lon2", "lat"),
                        by.y=c("lon", "lat"), all=T)
    
    plotDF.rev <- plotDF.rev[!is.na(plotDF.rev$BIOME),]
    
    ### prepare plotting labels
    plotDF.rev$BIOME2 <- as.character(plotDF.rev$BIOME)
    plotDF.rev$BIOME2 <- gsub("10", "j", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("11", "k", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("12", "l", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("13", "m", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("14", "n", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("1", "a", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("2", "b", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("3", "c", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("4", "d", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("5", "e", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("6", "f", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("7", "g", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("8", "h", plotDF.rev$BIOME2)
    plotDF.rev$BIOME2 <- gsub("9", "i", plotDF.rev$BIOME2)
    
    require(viridis)
    col.pal <- c(viridis(10), "red", "orange", "purple", "brown")

    
    ########################### perform statistical analysis ###########################
    require(sjstats)
    
    sumDF <- summaryBy(T_mean+T_sd+T_opt+T_param+
                           T_mean_weighted+T_sd_weighted+T_opt_weighted+T_param_weighted~BIOME,
                       FUN=c(mean, se), na.rm=T, data=plotDF.rev, keep.names=T)
    
    
    ########################### make plots ###########################
    
    ### Tgrowth
    p1 <- ggplot(plotDF.rev, aes(BIOME2, T_mean)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[growth] * " (" * degree * "C" * ")"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Topt
    p2 <- ggplot(plotDF.rev, aes(BIOME2, T_opt)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[opt] * " (" * degree * "C" * ")"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Tsd
    p3 <- ggplot(plotDF.rev, aes(BIOME2, T_sd)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[sd] * " (" * degree * "C" * ")"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Tparam
    p4 <- ggplot(plotDF.rev, aes(BIOME2, T_param)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ########################### prepare cos(lat) plot ##############################
    #### weighted by cos(lat)
    ### Tgrowth
    pw1 <- ggplot(plotDF.rev, aes(BIOME2, T_mean_weighted)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[growth] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Topt
    pw2 <- ggplot(plotDF.rev, aes(BIOME2, T_opt_weighted)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[opt] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Tsd
    pw3 <- ggplot(plotDF.rev, aes(BIOME2, T_sd_weighted)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression(T[sd] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ### Tparam
    pw4 <- ggplot(plotDF.rev, aes(BIOME2, T_param_weighted)) +
        geom_boxplot(aes(fill=BIOME2))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'vertical',
              legend.box.just = 'left')+
        ylab(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd] * " by cos(Lat)"))+
        scale_x_discrete(name="Biome", 
                         breaks=c("a", "b", "c", 
                                  "d", "e", "f",
                                  "g", "h", "i", 
                                  "j", "k", "l",
                                  "m", "n"), 
                         labels=c("TSMBF", "FSDBF", "TSCF", 
                                  "TBMF", "TCF", "BF", 
                                  "TSGSS", "TGSS", "FGS",
                                  "MGS", "T", "MFWS",
                                  "DXS", "M"))+
        scale_fill_manual(name="Biome",
                          limits=c("a", "b", "c", 
                                   "d", "e", "f",
                                   "g", "h", "i", 
                                   "j", "k", "l",
                                   "m", "n"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    ########################### plot distribution for each biome ##############################
    
    
    
    pdf(paste0(outdir, outname, "_biome_plot.pdf"),
        width=14, height=12)
    plot_grid(p1, p2, p3, p4,
              ncol=1, align="v", axis = "l",
              label_x=0.86, label_y=0.98,
              label_size = 18)
    dev.off()
    
    
}

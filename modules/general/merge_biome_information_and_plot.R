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
    

    plotDF.rev$BIOME3 <- as.character(plotDF.rev$BIOME2)
    plotDF.rev$BIOME3 <- gsub("a", "TSMBF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("b", "FSDBF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("c", "TSCF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("d", "TBMF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("e", "TCF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("f", "BF", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("g", "TSGSS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("h", "TGSS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("i", "FGS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("j", "MGS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("k", "T", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("l", "MFWS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("m", "DXS", plotDF.rev$BIOME3)
    plotDF.rev$BIOME3 <- gsub("n", "M", plotDF.rev$BIOME3)
    
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
    ## obtain inter-quantile range for each biome
    iqr <- IQR(plotDF.rev$T_param) 
    mean.value <- mean(plotDF.rev$T_param)
    
    
    p4 <- ggplot(plotDF.rev, aes(BIOME2, T_param)) +
        geom_boxplot(aes(fill=BIOME2), outlier.size=-1)+
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
        ylim(c(mean.value-(iqr*3), mean.value+(iqr*5)))+
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
    
    ########################### plot distribution for each biome #############################
    
    ### 1st method
    ph1 <- densityplot(~ T_mean | BIOME3, data = plotDF.rev,
                xlab = expression(T[growth] * " (" * degree * "C" * ")"),
                layout = c(4,4), fill=col.pal)
    
    ph2 <- densityplot(~ T_opt | BIOME3, data = plotDF.rev,
                       xlab = expression(T[opt] * " (" * degree * "C" * ")"),
                       layout = c(4,4), fill=col.pal)
    
    ph3 <- densityplot(~ T_sd | BIOME3, data = plotDF.rev,
                       xlab = expression(T[sd] * " (" * degree * "C" * ")"),
                       layout = c(4,4), fill=col.pal)
    
    ph4 <- densityplot(~ T_param | BIOME3, data = plotDF.rev,
                       xlab = expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]),
                       layout = c(4,4), fill=col.pal)

    ### 2nd method
    ### Tgrowth
    pha1 <- ggplot(plotDF.rev) +
        geom_density(aes(x=T_mean, group=BIOME2, fill=BIOME2), alpha=0.2)+
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
        xlab(expression(T[growth] * " (" * degree * "C" * ")"))+
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
        guides(color = guide_legend(nrow=5, byrow = T))
    
    ### Topt
    pha2 <- ggplot(plotDF.rev) +
        geom_density(aes(x=T_opt, group=BIOME2, fill=BIOME2), alpha=0.2)+
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
        xlab(expression(T[opt] * " (" * degree * "C" * ")"))+
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
        guides(color = guide_legend(nrow=5, byrow = T))
    
    ### Tsd
    pha3 <- ggplot(plotDF.rev) +
        geom_density(aes(x=T_sd, group=BIOME2, fill=BIOME2), alpha=0.2)+
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
        xlab(expression(T[sd] * " (" * degree * "C" * ")"))+
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
        guides(color = guide_legend(nrow=5, byrow = T))
    
    ### Tparam
    pha4 <- ggplot(plotDF.rev) +
        geom_density(aes(x=T_param, group=BIOME2, fill=BIOME2), alpha=0.2)+
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
        xlab(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))+
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
        guides(color = guide_legend(nrow=5, byrow = T))
    
    legend_shared <- get_legend(pha1 + theme(legend.position="bottom",
                                           legend.box = 'vertical',
                                           legend.box.just = 'left'))
    
    combined_plots <- plot_grid(pha1, pha2, pha3, pha4, 
                                labels=c("(a)", "(b)", "(c)", "(d)"),
                                ncol=1, align="vh", axis = "l",
                                label_x=0.88, label_y=0.95,
                                label_size = 18)
    
    
    ########################### output ############################
    ### biome specific plot
    pdf(paste0(outdir, outname, "_biome_plot.pdf"),
        width=14, height=12)
    plot_grid(p1, p2, p3, p4,
              ncol=1, align="v", axis = "l",
              label_x=0.86, label_y=0.98,
              label_size = 18)
    dev.off()
    
    
    #### distrubtion plot
    pdf(paste0(outdir, outname, "_biome_distribution_plot.pdf"))
    plot(ph1)
    plot(ph2)
    plot(ph3)
    plot(ph4)
    dev.off()
    
    
    ### alternative distribution plot
    pdf(paste0(outdir, outname, "_biome_alternative_distribution_plot.pdf"), width=6, height=10)
    plot_grid(combined_plots, legend_shared, ncol=1, rel_heights=c(1,0.1))
    dev.off()  
    
    
}

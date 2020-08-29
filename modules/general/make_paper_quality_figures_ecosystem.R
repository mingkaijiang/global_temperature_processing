make_paper_quality_figures_ecosystem <- function(plotDF, sd.filter.option,
                                                 outdir, outname) {
    
    ### create out directory
    if(!dir.exists(outdir)) {
      dir.create(outdir, showWarnings = FALSE)
    }
  
    ### read me
    ### sd.filter option: 
    ###                  no.filter: do not filter the SD
    ###                  filter: filter with criteria set within the function (i.e. > 1)
    
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
        
        plotDF$T_opt <- 0.76 * plotDF$T_mean + 6.48
        plotDF$T_param <- with(plotDF, T_opt - T_mean) / plotDF$T_sd
        
    } else if (sd.filter.option == "filter") {
        print("filter by replacing Tsd < 0.5 with 0.5")
        ### delete unreasonably small T sd
        plotDF <- plotDF[plotDF$T_sd >= 0.5, ]
        #plotDF$T_sd <- ifelse(plotDF$T_sd >= 0.5, plotDF$T_sd, 0.5)
        
        plotDF$T_opt <- 0.76 * plotDF$T_mean + 6.48
        plotDF$T_param <- with(plotDF, T_opt - T_mean) / plotDF$T_sd
        
    }
    
    
    ### remove T_mean < 0
    #plotDF$T_param <- ifelse(plotDF$T_mean <= 0, NA, plotDF$T_param)
    #plotDF$T_sd <- ifelse(plotDF$T_mean <= 0, NA, plotDF$T_sd)
    #plotDF$T_opt <- ifelse(plotDF$T_mean <= 0, NA, plotDF$T_opt)
    #plotDF$T_mean <- ifelse(plotDF$T_mean <= 0, NA, plotDF$T_mean)
    #
    #### remove NAs
    #plotDF <- plotDF[!is.na(plotDF$T_mean),]
    
    
    ########################### prepare TSM ~ Tsd ##############################
    ### calculate thermal safety margin
    plotDF$TSM <- with(plotDF, T_opt - T_mean)
    
    ### linear fit
    fit1 <- lm(TSM~T_sd, data=plotDF)
    
    ### plot
    p1 <- ggplot(plotDF, aes(x=T_sd, y = TSM)) + 
      geom_hex(bins = 80) +
      geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], col="red", lwd=2)+
      scale_fill_continuous(name = "No. of grids", type = "viridis") +
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=10),
            legend.title=element_text(size=10),
            panel.grid.major=element_blank(),
            legend.position = c(0.8, 0.25),
            plot.title = element_text(size = 10, face = "bold"))+
      scale_x_continuous(name=expression(T[sd] * " (" * degree * "C" * ")"))+
      scale_y_continuous(name=expression(T[opt] * " - " * T[growth] * " (" * degree * "C" * ")"))
    
    
    #plot(p1)
    
    
    ########################### prepare latitudinal pattern ##############################
    ### prepare latitudinal gradient DF
    latDF <- summaryBy(T_mean+T_sd+T_opt+T_param+TSM~lat, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=plotDF)
    
    latDF <- latDF[!is.na(latDF$T_mean.sd),]
    
    
    ### plot
    p4 <- ggplot(latDF, aes(x=lat, y=T_param.mean)) + 
      geom_ribbon(aes(x=lat, ymin=T_param.mean-T_param.sd,
                      ymax=T_param.mean+T_param.sd),
                  fill="grey")+
      geom_line(lwd = 2) +
      coord_flip()+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            plot.title = element_text(size = 10, face = "bold"))+
      scale_x_continuous("Latitude",
                         breaks=c(-65, -45, 0, 45, 90))+
      scale_y_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))
    
    
    #plot(p4)
    
    
    ########################### prepare pdf ##############################
    
    ### get the density
    myPDF <- as.vector(plotDF$T_param)
    d <- density(myPDF, na.rm=T)
    
    ### create data frame
    xd <- data.frame(d[c("x", "y")])
    
    ### find probability distribution marks
    probs <- c(0.025, 0.975)
    quantiles <- quantile(myPDF, prob=probs, na.rm=T)
    xd$quant <- factor(findInterval(xd$x,quantiles))
    
    
    p2 <- ggplot(xd, aes(x, y)) + 
        geom_line() +
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
      scale_x_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]),
                         breaks=c(0, 2.5, 5, 7.5, 10),
                         labels=c(0, 2.5, 5, 7.5, ">10"),
                         limits = c(-0.2, 10)) + 
      #geom_segment(aes(x = quantiles[1], xend = quantiles[1], 
      #                 y = 0.0, yend = 0.32), lty=2)+
      #geom_segment(aes(x = quantiles[2], xend = quantiles[2], 
      #                 y = 0.0, yend = 0.03), lty=2)+
      theme_linedraw() +
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=12),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            legend.position = c(0.8, 0.25),
            plot.title = element_text(size = 10, face = "bold"))+
      scale_y_continuous(name="Density")+
      scale_fill_manual(name = "Percentile",
                        breaks = c("0", "1", "2"),
                        labels = c("<2.5%", "2.5-97.5%", ">97.5"),
                        values=c("blue2", "yellow", "red2"))
        
    
    #p2 <- ggplot(plotDF, aes(x=T_param)) + 
    #    geom_density()+
    #    scale_x_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]),
    #                       breaks=c(0, 2.5, 5, 7.5, 10),
    #                       labels=c(0, 2.5, 5, 7.5, ">10"),
    #                       limits = c(-0.2, 10)) + 
    #    geom_segment(aes(x = quantiles[1], xend = quantiles[1], 
    #                     y = 0.0, yend = 0.32), lty=2)+
    #    geom_segment(aes(x = quantiles[2], xend = quantiles[2], 
    #                     y = 0.0, yend = 0.03), lty=2)+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"))+
    #    scale_y_continuous(name="Density")
    
    #plot(p2)
    
    ########################### prepare map ##############################
    ### prepare legend for continuous scale
    pal_continuous <- colorRampPalette(c("blue2", #"green", 
                                         "yellow", #"purple", 
                                         "red2"))
    
    n.discrete.color <- 3
    
    pal_discrete <- pal_continuous(n.discrete.color)
    
    probs <- c(0.0, 
               0.025, #0.25, 0.75, 
               0.975, 1.0)
    
    Tmean_brks <- quantile(plotDF$T_mean, prob=probs, na.rm=T)
    Topt_brks <- quantile(plotDF$T_opt, prob=probs, na.rm=T)
    Tsd_brks <- quantile(plotDF$T_sd, prob=probs, na.rm=T)
    Tparam_brks <- quantile(plotDF$T_param, prob=probs, na.rm=T)
    
    Tmean_brks[4] <- Tmean_brks[4] + 0.1
    Topt_brks[4] <- Topt_brks[4] + 0.1
    Tsd_brks[4] <- Tsd_brks[4] + 0.1
    Tparam_brks[4] <- Tparam_brks[4] + 0.1
    
    plotDF$Tmean_brks <- factor(findInterval(plotDF$T_mean,Tmean_brks))
    plotDF$Topt_brks <- factor(findInterval(plotDF$T_opt,Topt_brks))
    plotDF$Tsd_brks <- factor(findInterval(plotDF$T_sd,Tsd_brks))
    plotDF$Tparam_brks <- factor(findInterval(plotDF$T_param,Tparam_brks))
    
    
    #### create categorical plotting labels for each plotting variables
    plotDF$T_mean2 <- cut(plotDF$T_mean, 
                          breaks = Tmean_brks)
    
    plotDF$T_opt2 <- cut(plotDF$T_opt, 
                         breaks = Topt_brks)
    
    plotDF$T_sd2 <- cut(plotDF$T_sd, 
                        breaks = Tsd_brks)
    
    plotDF$T_param2 <- cut(plotDF$T_param, 
                           breaks = Tparam_brks)
    
    Tmean_lab <- levels(plotDF$T_mean2)
    Tmean_lab <- gsub(",", " to ", Tmean_lab)
    Tmean_lab <- gsub("]", "", Tmean_lab)
    Tmean_lab <- sub('.', '', Tmean_lab)
    
    Topt_lab <- levels(plotDF$T_opt2)
    Topt_lab <- gsub(",", " to ", Topt_lab)
    Topt_lab <- gsub("]", "", Topt_lab)
    Topt_lab <- sub('.', '', Topt_lab)
    
    Tsd_lab <- levels(plotDF$T_sd2)
    Tsd_lab <- gsub(",", " to ", Tsd_lab)
    Tsd_lab <- gsub("]", "", Tsd_lab)
    Tsd_lab <- sub('.', '', Tsd_lab)
    
    Tparam_lab <- levels(plotDF$T_param2)
    Tparam_lab <- gsub(",", " to ", Tparam_lab)
    Tparam_lab <- gsub("]", "", Tparam_lab)
    Tparam_lab <- sub('.', '', Tparam_lab)
    
    p3 <- ggplot() + 
      geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=Tparam_brks)) +
      coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
      scale_fill_manual(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
                        values = pal_discrete,
                        labels = Tparam_lab)+
      theme(panel.grid.minor=element_blank(),
            axis.title.x = element_text(size=14), 
            axis.text.x = element_text(size=12),
            axis.text.y=element_text(size=12),
            axis.title.y=element_text(size=14),
            legend.text=element_text(size=8),
            legend.title=element_text(size=12),
            panel.grid.major=element_blank(),
            plot.title = element_text(size = 10, face = "bold"),
            legend.position = c(0.15, 0.2),
            panel.background=element_rect(fill="white", colour="black"))+
      scale_x_continuous(name ="Longitude",
                         breaks=c(-180, -90, 0, 90, 180))+
      scale_y_continuous(name ="Latitude", 
                         breaks=c(-65, -45, 0, 45, 90))+
      guides(fill = guide_legend(ncol = 1, byrow = TRUE))
    
    #plot(p3)

    ########################### prepare biome plot ##############################
    ### overlay biome grids with plot grids
    DF1 <- plotDF[,c("lon2", "lat")]
    colnames(DF1) <- c("lon", "lat")
    
    require(viridis)
    
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
    
    
    plotDF.rev$BIOME4 <- as.character(plotDF.rev$BIOME3)
    plotDF.rev$BIOME4 <- gsub("TSMBF", "a", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("FSDBF", "a", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("TSCF", "a", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("TBMF", "b", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("TCF", "b", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("BF", "c", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("TSGSS", "d", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("TGSS", "e", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("FGS", "f", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("MGS", "g", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("T", "h", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("MFWS", "i", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("DXS", "j", plotDF.rev$BIOME4)
    plotDF.rev$BIOME4 <- gsub("M", "f", plotDF.rev$BIOME4)
    
    
    #col.pal <- c(viridis(10), "red", "orange", "purple", "brown")
    col.pal <- c(viridis(10))
    
    
    ### perform statistics
    require(sjstats)
    
    sumDF <- summaryBy(T_mean+T_sd+T_opt+T_param~BIOME,
                       FUN=c(mean, sd), na.rm=T, data=plotDF.rev, keep.names=T)
    
    
    ### Tparam
    ## obtain inter-quantile range for each biome
    iqr <- IQR(plotDF.rev$T_param) 
    mean.value <- mean(plotDF.rev$T_param)
    
    
    p5 <- ggplot(plotDF.rev, aes(BIOME4, T_param)) +
      geom_boxplot(aes(fill=BIOME4), outlier.size=-1)+
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
            legend.position = c(0.14, 0.2),
            legend.box.just = 'left')+
      ylim(c(0, mean.value+(iqr*5)))+
      ylab(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))+
      scale_x_discrete(name="Biome", 
                       breaks=c("a", "b", "c", 
                                "d", "e", "f",
                                "g", "h", "i", 
                                "j"), 
                       labels=c("TSF",   # tropical subtropical forest 
                                "TF",    # temperate forest
                                "BF",    # boreal forest
                                "TSGSS", # tropical subtropical grasses
                                "TGSS",  # temperate grasses
                                "FGSM",  # flooded grass and mangroves
                                "MGS",   # Montane grasses
                                "T",     # Tundra
                                "MFWS",  # Mediterranean
                                "DXS"))+ # Desert
      scale_fill_manual(name="Biome",
                        limits=c("a", "b", "c", 
                                 "d", "e", "f",
                                 "g", "h", "i", 
                                 "j"),
                        values = col.pal,
                        labels=c("TSF",   # tropical subtropical forest 
                                 "TF",    # temperate forest
                                 "BF",    # boreal forest
                                 "TSGSS", # tropical subtropical grasses
                                 "TGSS",  # temperate grasses
                                 "FGSM",  # flooded grass and mangroves
                                 "MGS",   # Montane grasses
                                 "T",     # Tundra
                                 "MFWS",  # Mediterranean
                                 "DXS"))+ # Desert
      guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    #plot(p5)
    
    
    ############################ prepare density plot split into biome ##############################
    p6 <- ggplot(plotDF.rev) +
      geom_density(aes(x=T_param, group=BIOME4, fill=BIOME4), alpha=0.2)+
      scale_x_continuous(breaks=c(0, 2.5, 5, 7.5, 10),
                         labels=c(0, 2.5, 5, 7.5, ">10"),
                         limits = c(-0.2, 10))+
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
                                 "j"),
                        values = col.pal,
                        labels=c("TSF",   # tropical subtropical forest 
                                 "TF",    # temperate forest
                                 "BF",    # boreal forest
                                 "TSGSS", # tropical subtropical grasses
                                 "TGSS",  # temperate grasses
                                 "FGSM",  # flooded grass and mangroves
                                 "MGS",   # Montane grasses
                                 "T",     # Tundra
                                 "MFWS",  # Mediterranean
                                 "DXS"))+ # Desert
      guides(color = guide_legend(nrow=5, byrow = T))
    
    #plot(p6)
    
    
    ### arrange screens
    first_row <- plot_grid(p1, p2, labels = c('(a)', '(b)'), label_size = 18,
                           label_x=c(0.12, 0.86), label_y=0.98)
    bottom_row <- plot_grid(p4, p5, labels = c("(d)", "(e)"), label_size  = 18, 
                            rel_widths = c(0.8, 1.2),
                            label_x=0.86, label_y=0.98)
    

    pdf(paste0(outdir, outname, "_Figure_1_ecosystem.pdf"),
        width=12, height=12)
    
    plot_grid(first_row, p3, bottom_row, nrow=3,
              ncol=1, align="v", axis = "l",
              rel_widths = c(1, 2.0, 1),
              rel_height = c(1, 1.5, 1),
              label_x=0.8, label_y=0.98,
              labels = c("", "(c)", ""),
              label_size = 18)
    dev.off()
        
    
    
}


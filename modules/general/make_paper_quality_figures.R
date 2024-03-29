make_paper_quality_figures <- function(plotDF, sd.filter.option,
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
    } else if (sd.filter.option == "filter") {
        print("filter by replacing Tsd < 0.5 with 0.5")
        ### delete unreasonably small T sd
        plotDF <- plotDF[plotDF$T_sd >= 0.5, ]
        #plotDF$T_sd <- ifelse(plotDF$T_sd >= 0.5, plotDF$T_sd, 0.5)
        
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
        geom_hex(bins = 10, color = "black") +
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
              legend.position = c(0.85, 0.25),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression("TSD (" * degree * "C" * ")"))+
        scale_y_continuous(name=expression(italic("T")[optA] * " - " * 
                                             italic("T")[growth] * " (" * degree * "C" * ")"))
    
    
    pdf(paste0(outdir, outname, "_hex_plot.pdf"),
        width=6, height=6)
    plot(p1)
    dev.off()
    
    ## latitude as a fill color - come back point 
    #p1 <- ggplot(plotDF, aes(x=T_sd, y = TSM, fill = lat)) + 
    #    geom_point(pch=21)+
    #    #geom_abline(intercept = coef(fit1)[1], slope = coef(fit1)[2], col="red", lwd=2)+
    #    geom_smooth(method="lm", se=T)+
    #    scale_fill_continuous(name = "Latitude", type = "viridis") +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=10),
    #          legend.title=element_text(size=10),
    #          panel.grid.major=element_blank(),
    #          legend.position = c(0.8, 0.25),
    #          plot.title = element_text(size = 10, face = "bold"))+
    #    scale_x_continuous(name=expression(T[sd] * " (" * degree * "C" * ")"))+
    #    scale_y_continuous(name=expression(T[opt] * " - " * T[growth] * " (" * degree * "C" * ")"))
    
    
    
    ########################### prepare latitudinal pattern ##############################
    ### prepare latitudinal gradient DF
    latDF <- summaryBy(T_mean+T_sd+T_opt+T_param+TSM~lat, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=plotDF)
    
    latDF <- latDF[!is.na(latDF$T_mean.sd),]
    
    
    ### plot
    p.lat1 <- ggplot(latDF, aes(x=lat, y=T_param.mean)) + 
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
                           breaks=c(-90, -45, 0, 45, 90),
                           limits = c(-55, 90))+
        scale_y_continuous(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"),
                           limits = c(0, 3.5))
    
    
    p.lat2 <- ggplot(latDF, aes(x=lat, y=T_param.mean)) + 
        geom_ribbon(aes(x=lat, ymin=T_param.mean-T_param.sd,
                        ymax=T_param.mean+T_param.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        coord_flip()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x=element_text(size=12),
              axis.title.y = element_blank(), 
              axis.text.x = element_text(size=12),
              axis.text.y.left=element_blank(),
              axis.text.y.right=element_text(size=12),
              plot.margin = margin(0, 0.2, 0.2, 0.2, "cm"),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous("Latitude",
                           breaks=c(-90, -45, 0, 45, 90),
                           limits = c(-55, 90),
                           sec.axis = dup_axis())+
        scale_y_continuous(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"),
                           limits = c(0, 3.5))
    

    pdf(paste0(outdir, outname, "_latitudinal.pdf"),
        width=2, height=6)
    plot(p.lat1)
    dev.off()
    
    
    ########################### prepare longitudinal pattern ##############################
    ### plot longitude summary
    lonDF <- summaryBy(T_mean+T_sd+T_opt+T_param+TSM~lon2, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=plotDF)
    
    lonDF <- lonDF[!is.na(lonDF$T_mean.sd),]
    
    
    ### plot
    p.lon1 <- ggplot(lonDF, aes(x=lon2, y=T_param.mean)) + 
        geom_ribbon(aes(x=lon2, ymin=T_param.mean-T_param.sd,
                        ymax=T_param.mean+T_param.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        #coord_flip()+
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
        scale_x_continuous("Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"),
                           limits = c(0, 3.0))
    
    
    p.lon2 <- ggplot(lonDF, aes(x=lon2, y=T_param.mean)) + 
        geom_ribbon(aes(x=lon2, ymin=T_param.mean-T_param.sd,
                        ymax=T_param.mean+T_param.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        #coord_flip()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x.top = element_text(size=12),
              axis.text.x.bottom = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous("Longitude",
                           breaks=c(-180, -90, 0, 90, 180),
                           sec.axis = dup_axis())+
        scale_y_continuous(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"),
                           limits = c(0, 3.0))
    
    
    plot(p.lon2)
    
    pdf(paste0(outdir, outname, "_longitudinal.pdf"),
        width=12, height=2)
    plot(p.lon1)
    dev.off()
    
    
    ### for each latitudinal band of 10, how does the parameter vary acorss longitude?
    ## assign latitudinal band of 10
    #plotDF$lat_band <- ifelse(plotDF$lat <= -45.0, -45.0, 
    #                          ifelse(plotDF$lat <= -15.0 & plotDF$lat > -45.0, -15.0,
    #                                 ifelse(plotDF$lat <= 15.0 & plotDF$lat > -15.0, 15.0,
    #                                        ifelse(plotDF$lat <= 45.0 & plotDF$lat > 15.0, 45.0,
    #                                               90.0))))
    #
    #latbandDF <- summaryBy(T_mean+T_sd+T_opt+T_param+TSM~lat_band+lon, 
    #                   FUN=c(mean, sd),
    #                   keep.names=T, na.rm=T, data=plotDF)
   
    
    
    ########################### prepare pdf ##############################
    col_discrete <- brewer.pal(7, "RdYlBu") 
    
    ### get the density
    myPDF <- as.vector(plotDF$T_param)
    d <- density(myPDF)
    
    ### create data frame
    xd <- data.frame(d[c("x", "y")])
    
    ### find probability distribution marks
    probs <- c(0.025, 0.05, 0.25, 0.75, 0.95, 0.975)
    quantiles <- quantile(myPDF, prob=probs)
    xd$quant <- factor(findInterval(xd$x,quantiles))
    
    p2 <- ggplot(xd, aes(x, y)) + 
        geom_line() +
        geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) + 
        scale_x_continuous(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"),
                           breaks=c(0, 1, 2, 3),
                           labels=c(0, 1, 2, 3),
                           limits = c(-0.2, 3.5)) + 
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = c(0.8, 0.65),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_y_continuous(name="Density")+
        scale_fill_manual(name = "Percentile",
                          #breaks = c("0", "1", "2", "3", "4"),
                          labels = c("< 2.5", "2.5-5.0", "5.0-25.0",
                                     "25.0-75.0", "75.0 - 95.0", "95.0-97.5", "> 97.5"),
                          values=col_discrete)
    
    
    pdf(paste0(outdir, outname, "_probability_distribution.pdf"),
        width=6, height=6)
    plot(p2)
    dev.off()
    
    
    
    ########################### prepare map ##############################
    ### prepare legend for continuous scale
    probs <- c(0, 0.025, 0.05, 0.25, 0.75, 0.95, 
               0.975, 1.0)
    
    Tmean_brks <- quantile(plotDF$T_mean, prob=probs)
    Topt_brks <- quantile(plotDF$T_opt, prob=probs)
    Tsd_brks <- quantile(plotDF$T_sd, prob=probs)
    Tparam_brks <- quantile(plotDF$T_param, prob=probs)
    
    Tmean_brks[8] <- Tmean_brks[8] + 0.1
    Topt_brks[8] <- Topt_brks[8] + 0.1
    Tsd_brks[8] <- Tsd_brks[8] + 0.1
    Tparam_brks[8] <- Tparam_brks[8] + 0.1
    
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
      scale_fill_manual(name=expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"), 
                        values = col_discrete,
                        labels = Tparam_lab)+
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
            legend.background = element_rect(fill=alpha("grey", 0.1),
                                             size=0.5, linetype="solid", 
                                             colour ="black"))+
      scale_x_continuous(name ="Longitude",
                         breaks=c(-180, -90, 0, 90, 180),
                         limits=c(-180, 180))+
      scale_y_continuous(name ="Latitude", 
                         breaks=c(-90, -45, 0, 45, 90),
                         limits=c(-90, 90))+
      guides(fill = guide_legend(ncol = 1, byrow = TRUE))
    
    
    pdf(paste0(outdir, outname, "_global_map.pdf"),
        width=12, height=6)
    plot(p3)
    dev.off()
    
    
    ########################### prepare biome plot ##############################
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
    
    col.pal <- c(viridis(10))
    
    
    ### perform statistics
    sumDF <- summaryBy(T_mean+T_sd+T_opt+T_param~BIOME,
                       FUN=c(mean, sd), na.rm=T, data=plotDF.rev, keep.names=T)
    
    
    ### Tparam
    ## obtain inter-quantile range for each biome
    iqr <- IQR(plotDF.rev$T_param) 
    mean.value <- mean(plotDF.rev$T_param)
    
    
    p5 <- ggplot(plotDF.rev, aes(BIOME4, T_param)) +
        geom_jitter(aes(fill=BIOME4), pch=21, alpha=0.2)+
        geom_boxplot(aes(fill=BIOME4), outlier.size=-1, color="black")+
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
        ylim(c(0, mean.value+(iqr*5)))+
        ylab(expression("(" * italic("T")[optA] * " - " * italic("T")[growth] * ")/TSD"))+
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
    
    
    pdf(paste0(outdir, outname, "_biome.pdf"),
        width=12, height=6)
    plot(p5)
    dev.off()
    
    
    
    ############################ prepare density plot split into biome ##############################
    #p6 <- ggplot(plotDF.rev) +
    #  geom_density(aes(x=T_param, group=BIOME4, fill=BIOME4), alpha=0.2)+
    #  scale_x_continuous(breaks=c(0, 1,2,3),
    #                     labels=c(0, 1,2,3),
    #                     limits = c(-0.2, 3.5))+
    #  theme_linedraw() +
    #  theme(panel.grid.minor=element_blank(),
    #        axis.text.x=element_text(size=12),
    #        axis.title.x=element_text(size=14),
    #        axis.text.y=element_text(size=12),
    #        axis.title.y=element_text(size=14),
    #        legend.text=element_text(size=12),
    #        legend.title=element_text(size=14),
    #        panel.grid.major=element_blank(),
    #        legend.position="none",
    #        legend.box = 'vertical',
    #        legend.box.just = 'left')+
    #  xlab(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))+
    #  scale_fill_manual(name="Biome",
    #                    limits=c("a", "b", "c", 
    #                             "d", "e", "f",
    #                             "g", "h", "i", 
    #                             "j"),
    #                    values = col.pal,
    #                    labels=c("TSF",   # tropical subtropical forest 
    #                             "TF",    # temperate forest
    #                             "BF",    # boreal forest
    #                             "TSGSS", # tropical subtropical grasses
    #                             "TGSS",  # temperate grasses
    #                             "FGSM",  # flooded grass and mangroves
    #                             "MGS",   # Montane grasses
    #                             "T",     # Tundra
    #                             "MFWS",  # Mediterranean
    #                             "DXS"))+ # Desert
    #  guides(color = guide_legend(nrow=5, byrow = T))
    
    
    ### arrange screens
    first_row <- plot_grid(p1, p2, labels = c('(a)', '(b)'), label_size = 18,
                           label_x=c(0.12, 0.86), label_y=0.98)
    bottom_row <- plot_grid(p4, p5, labels = c("(d)", "(e)"), label_size  = 18, 
                            rel_widths = c(0.8, 1.2),
                            label_x=0.86, label_y=0.98)
    
    
    ### plot first row only
    pdf(paste0(outdir, outname, "_Figure_2_first_row.pdf"),
        width=12, height=6)
    plot_grid(p1, p2, labels = c('(a)', '(b)'), label_size = 18,
          label_x=c(0.12, 0.86), label_y=0.98)
    dev.off()
    
    
    ### plot middle row only
    pdf(paste0(outdir, outname, "_Figure_2_middle_row.pdf"),
        width=12, height=6)
    
    ggarrange(p.lon2, NULL, 
              p3, p.lat2, 
              ncol = 2, nrow = 2,  #align = "hv", 
              widths = c(12, 2), heights = c(2, 6),
              common.legend = F,
              labels = c("(d)", "", "(c)", "(e)"),
              label.x = c(0.94, 1, 0.94, 0.05), 
              label.y = c(0.8, 1, 0.97, 0.97),
              font.label = list(size=18))
    dev.off()
    
    
    
    ### entire figure
    
    first_row <- plot_grid(p1, p2, labels = c('(a)', '(b)'), label_size = 18,
                               label_x=c(0.12, 0.86), label_y=0.98)
    
    second_row <- plot_grid(p.lon2, NULL, 
                            ncol = 2, 
                            rel_widths = c(1.0,0.2), 
                            labels = c("(d)", ""),
                            label_x = c(0.94, 1), 
                            label_y = c(0.8, 1),
                            label_size=18)
    
    third_row <- plot_grid(p3, p.lat2, 
                           ncol = 2, 
                           rel_widths = c(1.0,0.2), 
                           labels = c("(c)", "(e)"),
                           label_x = c(0.94, 0.05), 
                           label_y = c(0.97, 0.97),
                           label_size=18)
    
    ### final figure
    pdf(paste0(outdir, outname, "_Figure_2.pdf"),
        width=12, height=16)
    
    plot_grid(first_row, second_row, 
              third_row, p5, 
              nrow=4,
              ncol=1, #align="v", 
              axis = "l",
              rel_widths = c(1, 1, 1, 1),
              rel_heights = c(0.8, 0.2, 1.0, 0.8),
              label_x=0.94, label_y=0.98,
              labels = c("", "", "", "(f)"),
              label_size = 18)
    dev.off()
        
    
    
    
    ### alternative
    first_row <- plot_grid(p1, p2, labels = c('(a)', '(b)'), label_size = 18,
                           label_x=c(0.12, 0.86), label_y=0.98)
    
    second_row <- ggarrange(p.lon2, NULL, 
                            p3, p.lat2, 
                            ncol = 2, nrow = 2,  #align = "hv", 
                            widths = c(12, 2), heights = c(2, 6),
                            common.legend = F,
                            labels = c("(c)", "", "(d)", "(e)"),
                            label.x = c(0.94, 1, 0.94, 0.05), 
                            label.y = c(0.8, 1, 0.97, 0.97),
                            font.label = list(size=18))
    
  
    ### final figure
    pdf(paste0(outdir, outname, "_Figure_2.pdf"),
        width=12, height=16)
    
    plot_grid(first_row, second_row, 
              p5, 
              nrow=3,
              ncol=1, #align="v", 
              axis = "l",
              rel_widths = c(1, 1, 1),
              rel_heights = c(0.8, 1.0, 0.8),
              label_x=0.94, label_y=0.98,
              labels = c("", "", "(f)"),
              label_size = 18)
    dev.off()
    
    
}


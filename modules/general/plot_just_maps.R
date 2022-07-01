plot_just_maps <- function(plotDF, sd.filter.option,
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
    
    
    
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=Tmean_brks)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[growth]), 
                          values = col_discrete,
                          labels = Tmean_lab)+
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
    
    
    p2 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=Topt_brks)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[opt]), 
                          values = col_discrete,
                          labels = Topt_lab)+
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
    
    
    
    
    p3 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=Tsd_brks)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[sd]), 
                          values = col_discrete,
                          labels = Tsd_lab)+
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
    
    
    pdf(paste0(outdir, outname, "_global_map_Figure3.pdf"),
        width=12, height=14)
    plot_grid(p1, p2, p3,
              nrow=3,
              ncol=1, 
              axis = "l",
              #rel_widths = c(1, 1, 1),
              #rel_heights = c(0.8, 1.0, 0.8),
              label_x=0.94, label_y=0.98,
              labels = c("(a)", "(b)", "(c)"),
              label_size = 18)
    dev.off()
    
    
}
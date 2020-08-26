make_paper_quality_figure <- function(plotDF, sd.filter.option,
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
        print("filter by replacing Tsd < 1 with 1")
        ### delete unreasonably small T sd
        #plotDF <- plotDF[plotDF$T_sd >= 1.0, ]
        plotDF$T_sd <- ifelse(plotDF$T_sd >= 1.0, plotDF$T_sd, 1.0)
    }
    
    
    ### prepare latitudinal gradient DF
    latDF <- summaryBy(T_mean+T_sd+T_opt+T_param~lat, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=plotDF)
    
    ### prepare legend for continuous scale
    pal_continuous <- colorRampPalette(c("blue2", "yellow", "red3"))
    
    n.discrete.color <- 11
    
    pal_discrete <- pal_continuous(n.discrete.color)
    
    #### prepare discrete breaks
    Tmean_brks <- round(quantile(plotDF$T_mean, 
                                 probs = seq(0, 1, 
                                             (n.discrete.color-2)/100)), 2)
    Topt_brks <- round(quantile(plotDF$T_opt, 
                                probs = seq(0,1, 
                                            (n.discrete.color-2)/100)), 2)
    Tsd_brks <- round(quantile(plotDF$T_sd, 
                               probs = seq(0,1, 
                                           (n.discrete.color-2)/100)), 2)
    Tparam_brks <- round(quantile(plotDF$T_param, 
                                  probs = seq(0,1, 
                                              (n.discrete.color-2)/100)), 2)
    
    ### be more inclusive of the data
    Tmean_brks[1] <- Tmean_brks[1]-1.5
    Topt_brks[1] <- Topt_brks[1]-1.5
    Tsd_brks[1] <- Tsd_brks[1]-0.5
    Tparam_brks[1] <- Tparam_brks[1]-0.5
    
    Tmean_brks[n.discrete.color+1] <- Tmean_brks[n.discrete.color+1]+2.5
    Topt_brks[n.discrete.color+1] <- Topt_brks[n.discrete.color+1]+2.5
    Tsd_brks[n.discrete.color+1] <- Tsd_brks[n.discrete.color+1]+1.0
    Tparam_brks[n.discrete.color+1] <- Tparam_brks[n.discrete.color+1]+1.0
    
    ### round
    Tmean_brks <- round(Tmean_brks, 1)
    Topt_brks <- round(Topt_brks, 1)
    Tsd_brks <- round(Tsd_brks, 2)
    Tparam_brks <- round(Tparam_brks, 2)
    
    #### create categorical plotting labels for each plotting variables
    plotDF$T_mean2 <- cut(plotDF$T_mean, 
                          breaks = Tmean_brks)
    
    plotDF$T_opt2 <- cut(plotDF$T_opt, 
                         breaks = Topt_brks)
    
    plotDF$T_sd2 <- cut(plotDF$T_sd, 
                        breaks = Tsd_brks)
    
    plotDF$T_param2 <- cut(plotDF$T_param, 
                           breaks = Tparam_brks)
    
    ## remove NAs
    plotDF <- plotDF[!is.na(plotDF$T_param2),]
    
    ### brk labels
    Tmean_lab <- as.character(rev(unique(plotDF$T_mean2)))
    Tmean_lab <- gsub(",", " to ", Tmean_lab)
    Tmean_lab <- gsub("]", "", Tmean_lab)
    Tmean_lab <- sub('.', '', Tmean_lab)
    
    Topt_lab <- as.character(rev(unique(plotDF$T_opt2)))
    Topt_lab <- gsub(",", " to ", Topt_lab)
    Topt_lab <- gsub("]", "", Topt_lab)
    Topt_lab <- sub('.', '', Topt_lab)
    
    tmpDF <- plotDF[order(plotDF$T_sd), ]
    Tsd_lab <- as.character(unique(tmpDF$T_sd2))
    Tsd_lab <- gsub(",", " to ", Tsd_lab)
    Tsd_lab <- gsub("]", "", Tsd_lab)
    Tsd_lab <- sub('.', '', Tsd_lab)
    
    tmpDF <- plotDF[order(plotDF$T_param), ]
    Tparam_lab <- as.character(unique(tmpDF$T_param2))
    Tparam_lab <- gsub(",", " to ", Tparam_lab)
    Tparam_lab <- gsub("]", "", Tparam_lab)
    Tparam_lab <- sub('.', '', Tparam_lab)
    

    
    ########################### prepare density plot ##############################
    ### plotting
    pd1 <- ggplot(plotDF, aes(x=T_mean)) + 
        geom_density(fill="grey")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression(T[growth] * " (" * degree * "C" * ")"))+
        scale_y_continuous(name="Density")
    
    pd2 <- ggplot(plotDF, aes(x=T_opt)) + 
        geom_density(fill="grey")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression(T[opt] * " (" * degree * "C" * ")"))+
        scale_y_continuous(name="Density")
    
    
    pd3 <- ggplot(plotDF, aes(x=T_sd)) + 
        geom_density(fill="grey")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression(T[sd] * " (" * degree * "C" * ")"))+
        scale_y_continuous(name="Density")
    
    pd4 <- ggplot(plotDF, aes(x=T_param)) + 
        geom_density(fill="grey")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))+
        scale_y_continuous(name="Density")
    
    
    ########################### end density plot ##############################
    
    
    ########################### prepare latitudinal plot ##############################
    pl1 <- ggplot(latDF, aes(x=lat, y=T_mean.mean)) + 
        geom_ribbon(aes(x=lat, ymin=T_mean.mean-T_mean.sd,
                        ymax=T_mean.mean+T_mean.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        coord_flip()+
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
        scale_y_continuous(name=expression(T[growth] * " (" * degree * "C" * ")"))
    
    
    pl2 <- ggplot(latDF, aes(x=lat, y=T_opt.mean)) + 
        geom_ribbon(aes(x=lat, ymin=T_opt.mean-T_opt.sd,
                        ymax=T_opt.mean+T_opt.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        coord_flip()+
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
        scale_y_continuous(name=expression(T[opt] * " (" * degree * "C" * ")"))
    
    
    pl3 <- ggplot(latDF, aes(x=lat, y=T_sd.mean)) + 
        geom_ribbon(aes(x=lat, ymin=T_sd.mean-T_sd.sd,
                        ymax=T_sd.mean+T_sd.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        coord_flip()+
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
        scale_y_continuous(name=expression(T[sd] * " (" * degree * "C" * ")"))
    
    
    pl4 <- ggplot(latDF, aes(x=lat, y=T_param.mean)) + 
        geom_ribbon(aes(x=lat, ymin=T_param.mean-T_param.sd,
                        ymax=T_param.mean+T_param.sd),
                    fill="grey")+
        geom_line(lwd = 2) +
        coord_flip()+
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
    
    
    ########################### end latitudinal plot ##############################
    
    ########################### plot map continuous scale ##############################
    pm1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_mean)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_gradientn(name=expression(T[growth] * " (" * degree * "C" * ")             "),
                             colours = pal_continuous(9))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    pm2 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_opt)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_gradientn(name=expression(T[opt] * " (" * degree * "C" * ")             "), 
                             colours = pal_continuous(9))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    pm3 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_sd)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_gradientn(name=expression(T[sd] * " (" * degree * "C" * ")             "), 
                             colours = pal_continuous(9))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    
    pm4 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_param)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_gradientn(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
                             colours = pal_continuous(9))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    ########################### end map continuous scale ##############################

    pdf(paste0(outdir, outname, "_result_continuous_scale.pdf"),
        width=20, height=12)
    plot_grid(pm1, pd1, pl1, pd5,
              pm2, pd2, pl2, pd6,
              pm3, pd3, pl3, pd7,
              pm4, pd4, pl4, pd8,
              ncol=4, align="v", axis = "l",
              rel_widths=c(1, 0.5, 0.5, 0.5),
              label_x=0.86, label_y=0.98,
              label_size = 18)
    dev.off()
    
    ########################### end continuous scale ##############################

    
    ########################### plot map discrete scale ##############################
    pm5 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_mean2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[growth] * " (" * degree * "C" * ")             "),
                          values = pal_discrete,
                          labels = Tmean_lab)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=8),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    

    pm6 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_opt2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[opt] * " (" * degree * "C" * ")             "), 
                             values = pal_discrete,
                          labels = Topt_lab)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=8),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    
    pm7 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_sd2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[sd] * " (" * degree * "C" * ")             "), 
                          values = pal_discrete,
                          labels = Tsd_lab)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=8),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    pm8 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_param2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
                          values = pal_discrete,
                          labels = Tparam_lab)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=8),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        guides(fill = guide_legend(ncol = 2, byrow = TRUE))
    
    
    ########################### end map discrete scale ##############################
    
    pdf(paste0(outdir, outname, "_result_discrete_scale.pdf"),
        width=20, height=12)
    plot_grid(pm5, pd1, pl1, pd5,
              pm6, pd2, pl2, pd6,
              pm7, pd3, pl3, pd7,
              pm8, pd4, pl4, pd8,
              ncol=4, align="v", axis = "l",
              rel_widths=c(1, 0.5, 0.5, 0.5),
              label_x=0.86, label_y=0.98,
              label_size = 18)
    dev.off()
    
 
    
    
}


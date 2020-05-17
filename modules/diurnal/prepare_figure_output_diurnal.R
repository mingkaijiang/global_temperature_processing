prepare_figure_output_diurnal <- function(plotDF, sd.filter.option) {
    
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
    
    
    ### prepare cos(latitude) to weight the T value 
    plotDF$T_mean_weighted <- plotDF$T_mean / cos(plotDF$lat)
    plotDF$T_sd_weighted <- plotDF$T_sd / cos(plotDF$lat)
    plotDF$T_opt_weighted <- plotDF$T_opt / cos(plotDF$lat)
    plotDF$T_param_weighted <- plotDF$T_param / cos(plotDF$lat)
    
    
    ### prepare latitudinal gradient DF
    latDF <- summaryBy(T_mean+T_sd+T_opt+T_param~lat, 
                       FUN=c(mean, sd),
                       keep.names=T, na.rm=T, data=plotDF)
    
    ### prepare legend for continuous scale
    pal_continuous <- colorRampPalette(c("blue2", "yellow", "red3"))
    
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
    
    
    ### weighted by cos(latitude)
    pd5 <- ggplot(plotDF, aes(x=T_mean_weighted)) + 
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
        scale_x_continuous(name=expression(T[growth] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_y_continuous(name="Density")
    
    
    pd6 <- ggplot(plotDF, aes(x=T_opt_weighted)) + 
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
        scale_x_continuous(name=expression(T[opt] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_y_continuous(name="Density")
    
    
    pd7 <- ggplot(plotDF, aes(x=T_sd_weighted)) + 
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
        scale_x_continuous(name=expression(T[sd] * " (" * degree * "C" * ")" * " by cos(Lat)"))+
        scale_y_continuous(name="Density")
    
    
    pd8 <- ggplot(plotDF, aes(x=T_param_weighted)) + 
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
        scale_x_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd] * " by cos(Lat)"))+
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

    pdf("output/diurnal/diurnal_result_continuous_scale.pdf", width=20, height=12)
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
    
    
    
    ### create categorical plotting labels for each plotting variables
    Tmean_brks <- round(quantile(plotDF$T_mean, probs = seq(0,1, 0.2)), 2)
    plotDF$T_mean2 <- ifelse(plotDF$T_mean > Tmean_brks[1] & plotDF$T_mean <= Tmean_brks[2], "a", 
                             ifelse(plotDF$T_mean > Tmean_brks[2] & plotDF$T_mean <= Tmean_brks[3], "b",
                                    ifelse(plotDF$T_mean > Tmean_brks[3] & plotDF$T_mean <= Tmean_brks[4], "c", 
                                           ifelse(plotDF$T_mean > Tmean_brks[4] & plotDF$T_mean <= Tmean_brks[5], "d", "e"))))
    
    
    Tsd_brks <- round(quantile(plotDF$T_sd, probs = seq(0,1, 0.2)), 2)
    plotDF$T_sd2 <- ifelse(plotDF$T_sd > Tsd_brks[1] & plotDF$T_sd <= Tsd_brks[2], "a", 
                           ifelse(plotDF$T_sd > Tsd_brks[2] & plotDF$T_sd <= Tsd_brks[3], "b",
                                  ifelse(plotDF$T_sd > Tsd_brks[3] & plotDF$T_sd <= Tsd_brks[4], "c", 
                                         ifelse(plotDF$T_sd > Tsd_brks[4] & plotDF$T_sd <= Tsd_brks[5], "d", "e"))))
    
    
    Topt_brks <- round(quantile(plotDF$T_opt, probs = seq(0,1, 0.2)), 2)
    plotDF$T_opt2 <- ifelse(plotDF$T_opt > Topt_brks[1] & plotDF$T_opt <= Topt_brks[2], "a", 
                            ifelse(plotDF$T_opt > Topt_brks[2] & plotDF$T_opt <= Topt_brks[3], "b",
                                   ifelse(plotDF$T_opt > Topt_brks[3] & plotDF$T_opt <= Topt_brks[4], "c", 
                                          ifelse(plotDF$T_opt > Topt_brks[4] & plotDF$T_opt <= Topt_brks[5], "d", "e"))))
    
    ### make categorical ploting scheme
    Tstats_brks <- round(quantile(plotDF$stats, probs = seq(0,1, 0.2)), 2)
    plotDF$stats2 <- ifelse(plotDF$stats > Tstats_brks[1] & plotDF$stats <= Tstats_brks[2], "a", 
                            ifelse(plotDF$stats > Tstats_brks[2] & plotDF$stats <= Tstats_brks[3], "b",
                                   ifelse(plotDF$stats > Tstats_brks[3] & plotDF$stats <= Tstats_brks[4], "c", 
                                          ifelse(plotDF$stats > Tstats_brks[4] & plotDF$stats <= Tstats_brks[5], "d", "e"))))
    
    stats.mean <- mean(plotDF$stats, na.rm=T)
    stats.sd <- sd(plotDF$stats, na.rm=T)
    stats.min <- min(plotDF$stats, na.rm=T)
    stats.max <- max(plotDF$stats, na.rm=T)
    
    plotDF$stats3 <- cut(plotDF$stats, 
                         breaks = c(stats.min, 
                                    (stats.mean - stats.sd),
                                    stats.mean,
                                    (stats.mean + stats.sd),
                                    stats.max))
    
    
    ### set up plotting color discrete 
    col1 <- rev(brewer.pal(n = 5, name = "RdBu"))
    col.lab1 <- c(paste0(Tmean_brks[1], " to ", Tmean_brks[2]), 
                  paste0(Tmean_brks[2], " to ", Tmean_brks[3]), 
                  paste0(Tmean_brks[3], " to ", Tmean_brks[4]), 
                  paste0(Tmean_brks[4], " to ", Tmean_brks[5]),
                  paste0(Tmean_brks[5], " to ", Tmean_brks[6]))
    
    col2 <- brewer.pal(n = 5, name = "OrRd")
    col.lab2 <- c(paste0(Tsd_brks[1], " to ", Tsd_brks[2]), 
                  paste0(Tsd_brks[2], " to ", Tsd_brks[3]), 
                  paste0(Tsd_brks[3], " to ", Tsd_brks[4]), 
                  paste0(Tsd_brks[4], " to ", Tsd_brks[5]),
                  paste0(Tsd_brks[5], " to ", Tsd_brks[6]))
    
    col3 <- brewer.pal(n = 5, name = "YlGn")
    col.lab3 <- c(paste0(Topt_brks[1], " to ", Topt_brks[2]), 
                  paste0(Topt_brks[2], " to ", Topt_brks[3]), 
                  paste0(Topt_brks[3], " to ", Topt_brks[4]), 
                  paste0(Topt_brks[4], " to ", Topt_brks[5]),
                  paste0(Topt_brks[5], " to ", Topt_brks[6]))
    
    col4 <- brewer.pal(n = 5, name = "Blues")
    col.lab4 <- c(paste0(Tstats_brks[1], " to ", Tstats_brks[2]), 
                  paste0(Tstats_brks[2], " to ", Tstats_brks[3]), 
                  paste0(Tstats_brks[3], " to ", Tstats_brks[4]), 
                  paste0(Tstats_brks[4], " to ", Tstats_brks[5]),
                  paste0(Tstats_brks[5], " to ", Tstats_brks[6]))
    
    
    ########################### plot map discrete scale ##############################
    pm5 <- ggplot() + 
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
    
    pm6 <- ggplot() + 
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
    
    pm7 <- ggplot() + 
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
    
    
    pm8 <- ggplot() + 
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
    
    ########################### end map discrete scale ##############################
    
    pdf("output/diurnal/diurnal_result_discrete_scale.pdf", width=20, height=12)
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


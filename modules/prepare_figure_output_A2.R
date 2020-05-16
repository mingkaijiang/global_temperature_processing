prepare_figure_output_A2 <- function(landDF) {
    
    
    ########################### some modification to the dataset
    ### make a new DF
    plotDF <- landDF
    
    plotDF2 <- plotDF[!is.na(plotDF$T_sd),]
    
    ### delete unreasonably small T sd
    plotDF <- plotDF2[plotDF2$T_sd >= 0.05, ]
    
    
    ########################### prepare map output
    ### delete antarctica
    plotDF <- plotDF[plotDF$lat > -62, ]
    
    ### convert lon to make nicer plot
    plotDF$lon2 <- ifelse(plotDF$lon >180, (plotDF$lon - 360), plotDF$lon)
    
    
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

    require(sf)
    require(viridis)
    #library("rnaturalearth")
    #library("rnaturalearthdata")
    
    ### plot T growth mean
    p1 <- ggplot() + 
        #geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_mean2)) +
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_mean)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        #scale_fill_manual(name=expression(T[growth] * " (" * degree * "C" * ")"), 
        #                  values=col1,
        #                  label=col.lab1)+
        #scale_fill_viridis(option = "viridis")+
        scale_fill_viridis(option = "magma", 
                           guide=guide_colorbar(barwidth=20))+
        #scale_fill_viridis(option = "plasma")+
        #scale_fill_gradientn(colours = terrain.colors(10))+
        #scale_fill_gradientn(colours = rainbow(10))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_blank(),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="bottom")+
        scale_x_continuous(name ="Longitude",
                         breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                         breaks=c(-65, -45, 0, 45, 90))+
        ggtitle(expression(T[growth] * " (" * degree * "C" * ")"))
    
    plot(p1)

    ### plot T growth sd
    p2 <- ggplot() + 
        #geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_sd2)) +
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_sd)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        #scale_fill_manual(name=expression(T[sd] * " (" * degree * "C" * ")"), 
        #                  values=col2,
        #                  label=col.lab2)+
        scale_fill_viridis(option = "viridis", 
                           guide=guide_colorbar(barwidth=20))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_blank(),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="bottom")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        ggtitle(expression(T[sd] * " (" * degree * "C" * ")"))
    
    plot(p2)
    
    ### plot T opt
    p3 <- ggplot() + 
        #geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_opt2)) +
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_opt)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        #scale_fill_manual(name=expression(T[opt] * " (" * degree * "C" * ")"), 
        #                  values=col3,
        #                  label=col.lab3)+
        scale_fill_viridis(option = "viridis", 
                           guide=guide_colorbar(barwidth=20))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_blank(),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="bottom")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        ggtitle(expression(T[opt] * " (" * degree * "C" * ")"))
    
    
    ### plot statistics
    p4 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=stats2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
                          values=col4,
                          label=col.lab4)+
        #scale_fill_gradient(low = "black", high = "steelblue")+
        #stat_bin2d()+
        #stat_contour(data=plotDF, aes(x=lon2, y=lat, z=stats2,colour=..level..),size=0.5, bins=4)+
        #scale_colour_gradient(name=expression(paste(degree,"C",sep="")))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_blank(),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="bottom")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        ggtitle(expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))
    
    plot(p4)
    
    pdf("output/Maps_A2.pdf", width=12,height=16)
    plot_grid(p1, p2, p3, p4,
              labels=c("(a)", "(b)", "(c)", "(d)"), ncol=2, align="h", axis = "l")
    dev.off()
    
    
    ########################### prepare density plots
    ### prepare cos(latitude) to weight the responses
    plotDF$T_sd_weighted <- plotDF$T_sd / cos(plotDF$lat)
    plotDF$stats_weighted <- plotDF$stats / cos(plotDF$lat)
    
    
    ### plotting
    p5 <- ggplot(plotDF, aes(x=T_sd)) + 
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
    
    
    p6 <- ggplot(plotDF, aes(x=stats)) + 
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
    
    
    p7 <- ggplot(plotDF, aes(x=T_sd_weighted)) + 
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
        scale_x_continuous(name=expression(T[sd] * " weighted by cos (latitude)"))+
        scale_y_continuous(name="Density")
    
    
    p8 <- ggplot(plotDF, aes(x=stats_weighted)) + 
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
        scale_x_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd] * " weighted by cos (latitude)"))+
        scale_y_continuous(name="Density")
    
    
    p9 <- ggplot(plotDF, aes(x=lat, y=T_sd)) + 
        geom_bin2d(bins=100) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_fill_continuous(type = "viridis") +
        scale_x_continuous("Latitude")+
        scale_y_continuous(name=expression(T[sd] * " (" * degree * "C" * ")"))
    

    p10 <- ggplot(plotDF, aes(x=lat, y=stats)) + 
        geom_bin2d(bins=100) +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"))+
        scale_x_continuous("Latitude")+
        scale_fill_continuous(type = "viridis") +
        scale_y_continuous(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]))
    
    #plot(p10)
    
    
    pdf("output/Density_plots_A2.pdf", width=12,height=12)
    plot_grid(p5, p6, p7, p8, p9, p10,
              labels=c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), ncol=2, align="h", axis = "l")
    dev.off()
    
}


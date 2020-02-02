prepare_figure_output <- function(landDF) {
    
    
    ########################### some modification to the dataset
    ### make a new DF
    plotDF <- landDF
    
    ### delete unreasonably small T sd
    plotDF$stats <- ifelse(plotDF$stats >= 49, 49, plotDF$stats)
    
    
    
    ########################### prepare map output
    ### delete antarctica
    plotDF <- plotDF[plotDF$lat > -62, ]
    
    ### convert lon to make nicer plot
    plotDF$lon2 <- ifelse(plotDF$lon >180, (plotDF$lon - 360), plotDF$lon)
    
    
    ### create categorical plotting labels for each plotting variables
    plotDF$T_mean2 <- ifelse(plotDF$T_mean <= -20, "a", 
                             ifelse(plotDF$T_mean > -20 & plotDF$T_mean <= -10, "b",
                                    ifelse(plotDF$T_mean > -10 & plotDF$T_mean <= 0, "c", 
                                           ifelse(plotDF$T_mean > 0 & plotDF$T_mean <= 10, "d", 
                                                  ifelse(plotDF$T_mean > 10 & plotDF$T_mean <= 20, "e",
                                                         ifelse(plotDF$T_mean > 20 & plotDF$T_mean <= 30, "f", "g"))))))
    
    
    plotDF$T_sd2 <- ifelse(plotDF$T_sd <= 0.5, "a", 
                             ifelse(plotDF$T_sd > 0.5 & plotDF$T_sd <= 1, "b",
                                    ifelse(plotDF$T_sd > 1 & plotDF$T_sd <= 1.5, "c", 
                                           ifelse(plotDF$T_sd > 1.5 & plotDF$T_sd <= 2, "d", 
                                                  ifelse(plotDF$T_sd > 2.5 & plotDF$T_sd <= 3, "e", "f")))))
    
    
    plotDF$T_opt2 <- ifelse(plotDF$T_opt <= 0.0, "a", 
                           ifelse(plotDF$T_opt > 0.0 & plotDF$T_opt <=10, "b",
                                  ifelse(plotDF$T_opt > 10 & plotDF$T_opt <= 20, "c", 
                                         ifelse(plotDF$T_opt > 20 & plotDF$T_opt <= 30, "d",  "e"))))
    
    ### make categorical ploting scheme
    plotDF$stats2 <- ifelse(plotDF$stats <= 10, "a", 
                            ifelse(plotDF$stats > 10 & plotDF$stats <= 20, "b",
                                   ifelse(plotDF$stats > 20 & plotDF$stats <= 30, "c", "d")))
    
    
    ### set up plotting color discrete 
    col1 <- rev(brewer.pal(n = 7, name = "RdBu"))
    col.lab1 <- c("< -20", "-20 to -10", 
                  "-10 to 0", "0 to 10", "10 to 20", "20 to 30", "> 30")
    
    col2 <- brewer.pal(n = 6, name = "OrRd")
    col.lab2 <- c("0 to 0.5", "0.5 to 1.0", 
                  "1.0 to 1.5", "1.5 to 2.0", "2.0 to 2.5", "2.5 to 3.0")
    
    col3 <- brewer.pal(n = 5, name = "YlGn")
    col.lab3 <- c("< 0.0", "0 to 10", 
                  "10 to 20", "20 to 30", "> 30")
    
    require(viridis)
    require(scales)
    #show_col(viridis_pal()(4))
    #col4 <- c("#440154FF", "#31688EFF", "#35B779FF", "#FDE725FF")
    col4 <- brewer.pal(n = 4, name = "Blues")
    col.lab4 <- c("< 10", "10 to 20", 
                  "20 to 30", "> 30")

    ### plot T growth mean
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_mean2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[growth] * " (" * degree * "C" * ")"), 
                          values=col1,
                          label=col.lab1)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_x_continuous(name ="Longitude",
                         breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                         breaks=c(-65, -45, 0, 45, 90))

    ### plot T growth sd
    p2 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_sd2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[sd] * " (" * degree * "C" * ")"), 
                          values=col2,
                          label=col.lab2)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    
    ### plot T opt
    p3 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=T_opt2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression(T[opt] * " (" * degree * "C" * ")"), 
                          values=col3,
                          label=col.lab3)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    
    ### plot statistics
    p4 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon2, fill=stats2)) +
        coord_quickmap(xlim=range(plotDF$lon2), ylim=range(plotDF$lat))+
        scale_fill_manual(name=expression("(" * T[opt] * " - " * T[growth] * ")/" * T[sd]), 
                          values=col4,
                          label=col.lab4)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="right")+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))
    
    #plot(p4)
    
    #multi.panel.plot <-
    #    ggdraw() +
    #    draw_plot(p1, x = 0.0, y = .76, width = 1.0, height = .25) +
    #    draw_plot(p2, x = 0.0, y = .51, width = 1.0, height = .25) +
    #    draw_plot(p3, x = 0.0, y = .26, width = 1.0, height = .25) +
    #    draw_plot(p4, x = 0.0, y = .0, width = 1.0, height = .25)
    
    
    #ggsave(filename = "output/Topt_maps_based_on_Tmean.pdf", 
    #       plot = multi.panel.plot,
    #       width = 89, 
    #       height = 200,
    #       units = "mm",
    #       dpi = 300)
    
    pdf("output/Topt_maps_based_on_Tmean.pdf", width=12,height=16)
    plot_grid(p1, p2, p3, p4,
              labels=c("(a)", "(b)", "(c)", "(d)"), ncol=1, align="h", axis = "l")
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
    
    
    pdf("output/T_density_plots.pdf", width=12,height=12)
    plot_grid(p5, p6, p7, p8,
              labels=c("(a)", "(b)", "(c)", "(d)"), ncol=2, align="h", axis = "l")
    dev.off()
    
}


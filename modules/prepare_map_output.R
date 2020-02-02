prepare_map_output <- function(landDF) {
    
    ### make a new DF
    plotDF <- landDF
    
    ### delete unreasonably small T sd
    plotDF$stats <- ifelse(plotDF$stats >= 49, 49, plotDF$stats)
    
    ### make categorical ploting scheme
    plotDF$stats2 <- ifelse(plotDF$stats <= 10, "I", ifelse(plotDF$stats > 10 & plotDF$stats <= 20, "II",
                            ifelse(plotDF$stats > 20 & plotDF$stats <= 30, "III", ifelse(plotDF$stats > 30, "IV", "<0"))))
    
    
    ### plot T growth mean
    p1 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon, fill=T_mean)) +
        coord_quickmap(xlim=range(plotDF$lon), ylim=range(plotDF$lat))+
        scale_fill_continuous(name=expression(T[growth] * " " * degree * "C"), 
                          type="viridis")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position="right")
    
    ### plot T growth sd
    p2 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon, fill=T_sd)) +
        coord_quickmap(xlim=range(plotDF$lon), ylim=range(plotDF$lat))+
        scale_fill_continuous(name=expression(T[sd] * " " * degree * "C"), 
                              type="viridis")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position="right")
    
    ### plot T opt
    p3 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon, fill=T_opt)) +
        coord_quickmap(xlim=range(plotDF$lon), ylim=range(plotDF$lat))+
        scale_fill_continuous(name=expression(T[opt] * " " * degree * "C"), 
                              type="viridis")+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position="right")
    
    
    ### plot statistics
    p4 <- ggplot() + 
        geom_tile(data=plotDF, aes(y=lat, x=lon, fill=stats2)) +
        coord_quickmap(xlim=range(plotDF$lon), ylim=range(plotDF$lat))+
        #scale_fill_continuous(name=expression("risk param."), 
         #                     type="viridis")+
        #borders(colour = alpha("black", 0.8), lwd=0.2)+
        scale_fill_manual(name="stats", 
                          values=c("indianred4", "indianred1","thistle1", "skyblue"),
                          label=c("<10", "10-20", "20-30", ">30"))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position="right")
    
    #plot(p4)
    
    pdf("output/Topt_maps_based_on_Tmean.pdf", width=12,height=16)
    plot_grid(p1, p2, p3, p4,
              labels=c(""), ncol=1, align="h", axis = "l")
    dev.off()
    
    
}
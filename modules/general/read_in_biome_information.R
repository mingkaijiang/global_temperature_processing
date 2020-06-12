read_in_biome_information <- function(plot.option) {
    ### read in original shapefile
    #shp <- st_read("data/wwf_terr_ecos.shp")
    
    ### read in biome DF, at CRU resolution (i.e. 0.5 degree)
    biomeDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    subDF <- subset(biomeDF, BIOME >= 1 & BIOME <= 14)
    
    require(viridis)
    col.pal <- c(viridis(10), "red", "orange", "purple", "brown")
    
    ### need to project biomeDF onto 
    p1 <- ggplot(data=subDF, aes(y=lat, x=lon)) + 
        geom_tile(aes(fill=as.character(BIOME))) +
        coord_quickmap(xlim=range(subDF$lon), ylim=range(subDF$lat))+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 10, face = "bold"),
              legend.position="bottom",
              panel.background=element_rect(fill="black", colour="black"))+
        scale_x_continuous(name ="Longitude",
                           breaks=c(-180, -90, 0, 90, 180))+
        scale_y_continuous(name ="Latitude", 
                           breaks=c(-65, -45, 0, 45, 90))+
        scale_fill_manual(name="Biome",
                          limits=c("1", "2", "3", 
                                   "4", "5", "6",
                                   "7", "8", "9", 
                                   "10", "11", "12",
                                   "13", "14"),
                          values = col.pal,
                          labels=c("TSMBF", "FSDBF", "TSCF", 
                                   "TBMF", "TCF", "BF", 
                                   "TSGSS", "TGSS", "FGS",
                                   "MGS", "T", "MFWS",
                                   "DXS", "M"))+
        guides(fill = guide_legend(nrow=5, byrow = T))
    
    
    if(plot.option == T) {
        pdf("output/global_biome_categorization.pdf")
        plot(p1)
        dev.off()
    } 
    
    
    ### select data to output
    outDF <- subDF[,c("lon", "lat", "BIOME")]
    
    
    return(outDF)
    
    
}
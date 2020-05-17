merge_biome_information <- function(inDF) {
    
    ### read in original shapefile
    library(sf)
    require("rgdal") 
    require("maptools")
    require("plyr")
    
    shp <- st_read("data/wwf_terr_ecos.shp")
 
    
    
    ### read in biome DF, at CRU resolution (i.e. 0.5 degree)
    biomeDF <- read.csv("data/biome_temp_prec_full_1991_2012.csv")
    
    ### need to project biomeDF onto 
    
    
    p1 <- ggplot() + 
        geom_tile(data=biomeDF, aes(y=lat, x=lon, fill=as.factor(BIOME))) +
        coord_quickmap(xlim=range(biomeDF$lon), ylim=range(biomeDF$lat))+
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
    
    plot(p1)
    
    
    
}

get_elevation_data <- function() {
    
    
    require(elevatr)

    corDF <- plotDF[,c("lon2", "lat")]
    colnames(corDF) <- c("x","y")
    
    #ll_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    ll_prj <- "+init=EPSG:4326"
    
    cor.points <- sp::SpatialPoints(sp::coordinates(corDF),
                                    proj4string = sp::CRS(ll_prj))
    
    
    mans_sp <- SpatialPoints(coordinates(data.frame(x = -72.8145, y = 44.5438)),
                             CRS(ll_prj))
    mans <- get_elev_raster(locations =  mans_sp, z = 6)
    
    
    
    get_elev_point(locations=mts)
    
    
    
   ### end    
}
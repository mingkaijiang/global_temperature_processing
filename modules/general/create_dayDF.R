create_dayDF <- function(sourceDir,
                         destDir) {

    
    ### day list
    dnameDF <- data.frame(rep(c(1979:2018), each=12),
                          rep(c("jan", "feb", "mar", "apr", "may", "jun",
                                "jul", "aug", "sep", "oct", "nov", "dec"), by = 40),
                          NA, NA, NA, NA)
    colnames(dnameDF) <- c("year", "month", "yrmonth", "length", "s.loc", "e.loc")
    dnameDF$yrmonth <- paste(dnameDF$year, dnameDF$month, sep="_")
    
    ### create dname.list
    dname.list <- as.vector(dnameDF$yrmonth)
    
    loc.s <- 1
    
    for (j in 1:length(dname.list)) {
        
        ### name
        dname <- dname.list[j]
        
        #### read in data
        inName <- paste0(sourceDir, "era_interim_2m_temperature_6_hourly_", dname, ".nc") 
        
        ### open nc file
        nc <- nc_open(inName)
        
        ### get length
        time <- ncvar_get(nc, "time") # hours since 1900-01-01 00:00:00.0
        ntime <- length(time)
        loc.e <- loc.s+ntime - 1
        
        nc_close(nc)
        
        ### assign
        dnameDF$length[j] <- ntime
        dnameDF$s.loc[j] <- loc.s
        dnameDF$e.loc[j] <- loc.e
            
        ### update loc.s
        loc.s <- loc.e + 1
        
    }
    
    write.csv(dnameDF, paste0(destDir, "/dateDF.csv"),
              row.names=F)

    
}
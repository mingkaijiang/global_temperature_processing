prepare_ERA_INTERIM_dataset_and_split <- function(sourceDir,
                                                  dname,
                                                  destDir) {
    
    ###########################################################
    #### define lon lat and time information
    dim1 <- 480
    dim2 <- 241
    day.list <- seq.Date(as.Date("1979/01/01"), 
                         as.Date("2018/03/31"), 
                         by="day")
    dim3 <- length(day.list) * 4
    
    ### split sequence:
    split.seq <- seq(40, 480, 40)
    out.file.group <- 1:length(split.seq)
    
    ### create storage DF
    tmp <- array(0, c(40, dim2, dim3))

    ###########################################################
    ### loop through to store the data
    for (i in out.file.group) {
        
        ## lon location
        lon.loc1 <- split.seq[i] - 39
        lon.loc2 <- split.seq[i] 
        
        # time location
        time.loc1 <- 1
        
        for (j in 1:length(dname.list)) {
            #### read in data
            inName <- paste0(sourceDir, "era_interim_2m_temperature_6_hourly_", dname, ".nc") 
            
            ### open nc file
            nc <- nc_open(inName)
            
            ### get length
            time <- ncvar_get(nc, "time") # hours since 1900-01-01 00:00:00.0
            ntime <- length(time)
            time.loc2 <- time.loc1+ntime - 1
            
            ### read in the 3d file
            tmp_array <- ncvar_get(nc,"t2m")
            
            ### assign values
            tmp[,,time.loc1:time.loc2] <- tmp_array[lon.loc1:lon.loc2,,]
            
            ### update time.loc1
            time.loc1 <- time.loc2 + 1
            
        }
        
        ### save output
        saveRDS(tmp, file=paste0(destDir, "/Group_", i, ".rds"))
        
    }
    
    
    
    
}
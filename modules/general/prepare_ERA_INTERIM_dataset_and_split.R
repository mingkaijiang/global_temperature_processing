prepare_ERA_INTERIM_dataset_and_split <- function(sourceDir,
                                                  destDir) {
    
    ### create out directory
    if(!dir.exists(destDir)) {
        dir.create(destDir, showWarnings = FALSE)
    }
    
    ###########################################################
    #### define lon lat and time information
    dim1 <- 480
    dim2 <- 241
    dayDF <- read.csv("output/dateDF.csv")
    dname.list <- as.vector(dayDF$yrmonth)
    dim3 <- dayDF$e.loc[dim1]
    
    ### split sequence:
    split.seq <- seq(20, dim1, 20)
    out.file.group <- 1:length(split.seq)
    
    ### create storage DF
    tmp <- array(0, c(20, dim2, dim3))

    ###########################################################
    ### loop through to store the data
    for (i in out.file.group) {
        
        ## lon location
        lon.loc1 <- split.seq[i] - 19
        lon.loc2 <- split.seq[i] 
        
        for (j in 1:length(dname.list)) {
            
            dname <- dname.list[j]
            
            #### read in data
            inName <- paste0(sourceDir, "era_interim_2m_temperature_6_hourly_", dname, ".nc") 
            
            ### open nc file
            nc <- nc_open(inName)
            
            ### read in the 3d file
            tmp_array <- ncvar_get(nc,"t2m")
            
            ### time loc
            time.loc1 <- dayDF$s.loc[j]
            time.loc2 <- dayDF$e.loc[j]
            
            ### assign values
            tmp[,,time.loc1:time.loc2] <- tmp_array[lon.loc1:lon.loc2,,]
            
            ### close
            nc_close(nc)
        }
        
        ### save output
        saveRDS(tmp, file=paste0(destDir, "Group_", i, ".rds"))
        
    }
    
    
}
merge_splitted_files_and_remove_sea_surface <- function(sourceDir, 
                                                        destDir,
                                                        return.option) {
  
  ### create out directory
  if(!dir.exists(destDir)) {
    dir.create(destDir, showWarnings = FALSE)
  }
  
  ### prepare storage DF
  mgDF <- array(NA, c(480, 241, 4))
  
  ### list all files
  file.list <- list.files(path = sourceDir, pattern = ".rds")
  
  for (i in 1:length(file.list)) {
    inDF <- readRDS(paste0(sourceDir, "Group_", i, "_temperature_parameters.rds"))
    
    s <- (i - 1) * 20 + 1
    e <- i * 20
      
    mgDF[s:e,,] <- inDF
    
  }
  
  ### save output in RDS format
  saveRDS(mgDF, paste0(destDir, "merged_temperature_parameters.rds"))
  
  ### read in lonlat information
  lonlatDF <- read.csv("output/lonlat.csv")
  
  ### prepare lonlatDF
  lonlatDF$T_mean <- as.vector(mgDF[,,1])
  lonlatDF$T_sd <- as.vector(mgDF[,,2])
  lonlatDF$T_opt <- as.vector(mgDF[,,3])
  lonlatDF$T_param <- as.vector(mgDF[,,4])
  
  
  ### prepare sea surface area mask
  ssfDF <- read_sea_surface_mask()
  
  ### merge ssf and TgrDF
  mgDF <- merge(lonlatDF, ssfDF, by=c("lon", "lat"))
  
  ### subtract only land
  landDF <- mgDF[is.na(mgDF$ssf),]
  
  ### return
  return(landDF)
  
}
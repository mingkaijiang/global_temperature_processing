calculate_gridded_temperature_parameters <- function (sourceDir, 
                                                      destDir,
                                                      return.option) {
  
    ### create out directory
    if(!dir.exists(destDir)) {
      dir.create(destDir, showWarnings = FALSE)
    }
  
    ### list all files
    file.list <- list.files(path = sourceDir, pattern = ".rds")
    
    ### read in dayDF
    dayDF <- read.csv("output/dateDF.csv")
    
    ### prepare output storage
    out <- array(NA, c(20, 241, 4))
    
    ### loop through each file
    for (i in 1:length(file.list)) {
      ### read input
      myDF <- readRDS(paste0(sourceDir, "Group_", i, ".rds"))
      
      ### convert unit from K to degree C
      myDF <- myDF - 273.15
      
      ### calculate Tmean and Tsd
      ### now we have two return options:
      ### 1. calculate grow season temperature, i.e., mean temperature of months when T > 0 
      ### 2. calculate annual mean temperature
      if (return.option == "growth") {
        print("no code written")
        
      } else if (return.option == "annual") {
        ### Tmean based on all data
        Tmean <- rowMeans(myDF, dims = 2, na.rm=T)
        
        ### Tsd based on all data
        Tsd <- apply(myDF, c(1,2), sd)
        
        ### calculate Topt
        Topt <- 13.9 + 0.61 * Tmean
        
        ### test statistic
        Tparam <- (Topt - Tmean) / Tsd
        
        ### save all to out
        out[,,1] <- Tmean
        out[,,2] <- Tsd
        out[,,3] <- Topt
        out[,,4] <- Tparam
        
        saveRDS(out, file=paste0(destDir, "Group_", i, "_temperature_parameters.rds"))
        
      } # if statement for return.option
      
    } # for statement for file loop
  
}
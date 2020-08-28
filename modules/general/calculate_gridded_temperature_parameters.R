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
    
    ### prepare a yearDF
    sDF <- summaryBy(s.loc~year, FUN=min, data=dayDF, na.rm=T, keep.names=T)
    eDF <- summaryBy(e.loc~year, FUN=max, data=dayDF, na.rm=T, keep.names=T)
    
    yrDF <- merge(sDF, eDF, by="year")
    
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
      if (return.option == "hourly") {
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
          
          ### save output
          saveRDS(out, file=paste0(destDir, "Group_", i, "_temperature_parameters.rds"))
        
      } else if (return.option == "daily") {
          ## create storage DF to store the daily means
          tmpDF <- array(NA, c(20, 241, 14610))
          
          ## calculate daily means based on 6-hourly data
          for (j in 1:20) {
              for (k in 1:241) {
                  tmpDF2 <- data.frame(myDF[j,k,], rep(c(1:14610), each=4))
                  colnames(tmpDF2) <- c("value", "group")
                  avgDF <- summaryBy(value~group, data=tmpDF2, keep.names=T, na.rm=T)
                  tmpDF[j,k,] <- avgDF$value
              }
          }
          
          ### Tmean based on all data
          Tmean <- rowMeans(tmpDF, dims = 2, na.rm=T)
          
          ### Tsd based on all data
          Tsd <- apply(tmpDF, c(1,2), sd)
          
          ### calculate Topt
          Topt <- 13.9 + 0.61 * Tmean
          
          ### test statistic
          Tparam <- (Topt - Tmean) / Tsd
          
          ### save all to out
          out[,,1] <- Tmean
          out[,,2] <- Tsd
          out[,,3] <- Topt
          out[,,4] <- Tparam
          
          ### save output
          saveRDS(out, file=paste0(destDir, "Group_", i, "_temperature_parameters.rds"))
        
      } else if (return.option == "monthly") {
          ## create storage DF to store the monthly means
          tmpDF <- array(NA, c(20, 241, 480))
          
          ## calculate monthly means based on 6-hourly data
          for (j in 1:20) {
              for (k in 1:241) {
                  for (l in 1:480) {
                      s <- dayDF[l,"s.loc"]
                      e <- dayDF[l, "e.loc"]
                      tmpDF[j,k,l] <- mean(myDF[j,k,s:e], na.rm=T)
                  }
              }
          }
          
          ### Tmean based on all data
          Tmean <- rowMeans(tmpDF, dims = 2, na.rm=T)
          
          ### Tsd based on all data
          Tsd <- apply(tmpDF, c(1,2), sd)
          
          ### calculate Topt
          Topt <- 13.9 + 0.61 * Tmean
          
          ### test statistic
          Tparam <- (Topt - Tmean) / Tsd
          
          ### save all to out
          out[,,1] <- Tmean
          out[,,2] <- Tsd
          out[,,3] <- Topt
          out[,,4] <- Tparam
          
          ### save output
          saveRDS(out, file=paste0(destDir, "Group_", i, "_temperature_parameters.rds"))
          
          
      } else if (return.option == "annual") {
          ## create storage DF to store the monthly means
          tmpDF <- array(NA, c(20, 241, 40))
          
          ## calculate annual means based on 6-hourly data
          for (j in 1:20) {
              for (k in 1:241) {
                  for (l in 1:40) {
                      s <- yrDF[l,"s.loc"]
                      e <- yrDF[l, "e.loc"]
                      tmpDF[j,k,l] <- mean(myDF[j,k,s:e], na.rm=T)
                  }
              }
          }
          
          ### Tmean based on all data
          Tmean <- rowMeans(tmpDF, dims = 2, na.rm=T)
          
          ### Tsd based on all data
          Tsd <- apply(tmpDF, c(1,2), sd)
          
          ### calculate Topt
          Topt <- 13.9 + 0.61 * Tmean
          
          ### test statistic
          Tparam <- (Topt - Tmean) / Tsd
          
          ### save all to out
          out[,,1] <- Tmean
          out[,,2] <- Tsd
          out[,,3] <- Topt
          out[,,4] <- Tparam
          
          ### save output
          saveRDS(out, file=paste0(destDir, "Group_", i, "_temperature_parameters.rds"))
          
          
      } else {
          print("no option")
      } # if statement for return.option
      
    } # for statement for file loop
  
}
prepare_diurnal_output <- function(meanDF, sdDF, nDF, 
                                   annDF, ssfDF, dname.list, 
                                   return.option) {
    
    ### set colnames for the monthly mean, sd and n DF
    colnames(meanDF) <- c("lon", "lat", paste0("m_", dname.list))
    colnames(sdDF) <- c("lon", "lat", paste0("s_", dname.list))
    colnames(nDF) <- c("lon", "lat", paste0("n_", dname.list))
    
    ### prepare data to calculate overall mean, sds
    mean.matrix <- as.matrix(meanDF[,-c(1:2)])
    sd.matrix <- as.matrix(sdDF[,-c(1:2)])
    n.matrix <- as.matrix(nDF[,-c(1:2)])
    
    ### convert unit from K to C
    mean.matrix <- mean.matrix - 273.15
    
    ### get matrix dimension information to check number of years of data included
    n.yr <- dim(mean.matrix)[2]/12
    
    ### now we have two return options:
    ### 1. calculate grow season temperature, i.e., mean temperature of months when T > 0 
    ### 2. calculate annual mean temperature
    if (return.option == "growth") {
        ### calculate growth temperature as months when T > 0
        
        tmpDF2 <- ifelse(mean.matrix >=0, mean.matrix, "")
        tmpDF2 <- apply(tmpDF2, 2, as.numeric)
        
        tmpDF3 <- ifelse(mean.matrix >= 0, sd.matrix, "")
        tmpDF3 <- apply(tmpDF3, 2, as.numeric)
        
        tmpDF4 <- ifelse(mean.matrix >= 0, n.matrix, "")
        tmpDF4 <- apply(tmpDF4, 2, as.numeric)
        
        
        ### calculate row means
        test1 <- tmpDF2 * tmpDF4
        test2 <- rowSums(test1, na.rm=T)
        test3 <- rowSums(tmpDF4, na.rm=T)
        test4 <- test2/test3
        annDF$T_mean <- test4
        
        ### calculate row sd
        test1 <- tmpDF3 * tmpDF3
        test2 <- tmpDF4 - 1
        test3 <- test1 * test2
        test4 <- rowSums(test3, na.rm=T)
        test5 <- rowSums(tmpDF4, na.rm=T)
        test6 <- tmpDF4/tmpDF4
        test7 <- rowSums(test6, na.rm=T)
        test8 <- test5 - test7
        test9 <- sqrt(test4/test8)
        
        annDF$T_sd <- test9
        
        outDF <- data.frame(annDF[,c(1:4)], n.yr)
        colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "n_year")
        
        ### calculate Topt
        outDF$T_opt <- 13.9 + 0.61 * outDF$T_mean
        
        ### test statistic
        outDF$T_param <- with(outDF, (T_opt - T_mean) / T_sd)
        
        ### merge ssf and TgrDF
        mgDF <- merge(outDF, ssfDF, by=c("lon", "lat"))
        
        ### subtract only land
        landDF <- mgDF[is.na(mgDF$ssf),]
        
        ### write csv
        write.csv(landDF, "output/diurnal/Tmean_grow_season.csv", row.names=F)
        
    } else if (return.option == "annual") {
        ### return annual mean T for each year
        
        ### calculate annual Tmean
        test1 <- mean.matrix * n.matrix
        test2 <- rowSums(test1, na.rm=T)
        test3 <- rowSums(n.matrix, na.rm=T)
        test4 <- test2/test3
        annDF$T_mean <- test4
        
        ### calculate annual T based on monthly T
        ### calculate row sd
        test1 <- sd.matrix * sd.matrix
        test2 <- n.matrix - 1
        test3 <- test1 * test2
        test4 <- rowSums(test3, na.rm=T)
        test5 <- rowSums(n.matrix, na.rm=T)
        test6 <- n.matrix/n.matrix
        test7 <- rowSums(test6, na.rm=T)
        test8 <- test5 - test7
        test9 <- sqrt(test4/test8)
        
        annDF$T_sd <- test9
        
        ### colnames
        outDF <- data.frame(annDF[,c(1:4)], n.yr)
        colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "n_year")
        
        ### calculate Topt
        outDF$T_opt <- 13.9 + 0.61 * outDF$T_mean
        
        ### test statistic
        outDF$T_param <- with(outDF, (T_opt - T_mean) / T_sd)
        
        ### merge ssf and TgrDF
        mgDF <- merge(outDF, ssfDF, by=c("lon", "lat"))
        
        ### subtract only land
        landDF <- mgDF[is.na(mgDF$ssf),]
        
        ### write csv
        write.csv(landDF, "output/diurnal/Tmean_annual.csv", row.names=F)
    }
    
    
    ### return
    return(landDF)
}
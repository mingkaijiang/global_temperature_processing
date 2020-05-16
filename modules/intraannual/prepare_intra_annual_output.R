prepare_intra_annual_output <- function(meanDF, 
                                        annDF, ssfDF, dname.list, 
                                        return.option) {
    
    ### set colnames for the summaryDFs
    colnames(meanDF) <- c("lon", "lat", paste0("m_", dname.list))
    

    ### prepare data to calculate overall mean, sds
    mean.matrix <- as.matrix(meanDF[,-c(1:2)])
    
    ### convert unit from K to C
    mean.matrix <- mean.matrix - 273.15
    
    
    ### get matrix dimension information to check number of years of data included
    n.yr <- dim(mean.matrix)[2]/12
    
    
    ### now we have two return options:
    ### 1. calculate grow season temperature, i.e., mean temperature of months when T > 0 
    ### 2. calculate annual mean temperature
    if (return.option == "growth") {
        ### calculate growth temperature as months when T > 0
        
        tmpDF2 <- ifelse(mean.matrix >= 0, mean.matrix, "")
        tmpDF2 <- apply(tmpDF2, 2, as.numeric)
        
        annDF$T_mean <- rowMeans(tmpDF2, na.rm=T)
        annDF$T_sd <- rowSds(tmpDF2, na.rm=T)
        
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
        write.csv(landDF, "output/intraannual/Tmean_grow_season.csv", row.names=F)
        
    } else if (return.option == "annual") {
        ### return annual mean T for each year
        
        ### calculate annual T based on monthly T
        annDF$T_mean <- rowMeans(mean.matrix, na.rm=T)
        annDF$T_sd <- rowSds(mean.matrix, na.rm=T)
        
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
        write.csv(landDF, "output/intraannual/Tmean_annual.csv", row.names=F)
    }
    
 
    ### return
    return(landDF)
}
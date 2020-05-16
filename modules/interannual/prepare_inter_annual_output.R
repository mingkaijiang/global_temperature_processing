prepare_inter_annual_output <- function(meanDF, 
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
        
        ### subset each year
        for (i in 1:n.yr) {
            tmpDF <- mean.matrix[,(1+12*(i-1)):(12*i)]
            
            tmpDF2 <- ifelse(tmpDF >=0, tmpDF, "")
            tmpDF2 <- apply(tmpDF2, 2, as.numeric)
            
            ann.meanDF <- rowMeans(tmpDF2, na.rm=T)
            annDF[,2+i] <- ann.meanDF
        }
        
        ann.meanDF <- rowMeans(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)
        ann.stdevDF <- rowSds(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)
        
        outDF <- data.frame(annDF[,c(1:2)], ann.meanDF, ann.stdevDF, n.yr)
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
        write.csv(landDF, "output/interannual/Tmean_grow_season.csv", row.names=F)
        
    } else if (return.option == "annual") {
        ### return annual mean T for each year
        
        ### subset each year
        for (i in 1:n.yr) {
            tmpDF <- mean.matrix[,(1+12*(i-1)):(12*i)]
            
            ### calculate annual T based on monthly T
            ann.meanDF <- rowMeans(tmpDF, na.rm=T)
            
            ### assign to output df
            annDF[,2+i] <- ann.meanDF
        }
        
        ann.meanDF <- rowMeans(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)
        ann.stdevDF <- rowSds(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)
        
        outDF <- data.frame(annDF[,c(1:2)], ann.meanDF, ann.stdevDF, n.yr)
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
        write.csv(landDF, "output/interannual/Tmean_annual.csv", row.names=F)
    }
    
 
    ### return
    return(landDF)
}
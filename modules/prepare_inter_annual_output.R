prepare_inter_annual_output <- function(meanDF, sdDF, nDF, annDF, 
                                        dname.list, return.option) {
    
    ### set colnames for the summaryDFs
    colnames(meanDF) <- c("lon", "lat", paste0("m_", dname.list))
    colnames(sdDF) <- c("lon", "lat", paste0("s_", dname.list))
    colnames(nDF) <- c("lon", "lat", paste0("n_", dname.list))
    
    ### save monthly mean, sd and sample size csv
    write.csv(meanDF, "output/monthly_mean.csv", row.names=F)
    #write.csv(sdDF, "output/monthly_sd.csv", row.names=F)
    #write.csv(nDF, "output/monthly_sample_size.csv", row.names=F)
    
    
    ### prepare data to calculate overall mean, sds
    mean.matrix <- as.matrix(meanDF[,-c(1:2)])
    sd.matrix <- as.matrix(sdDF[,-c(1:2)])
    n.matrix <- as.matrix(nDF[,-c(1:2)])
    
    ### convert unit from K to C
    mean.matrix <- mean.matrix - 273.15
    
    
    ### get matrix dimension information to check number of years of data included
    n.yr <- dim(mean.matrix)[2]/12
    
    
    ### now we have two return options:
    ### either calculate growth temperature, that is, mean temperature of months when T > 0 
    ### or, calculate annual mean temperature
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
    }
    
    ann.meanDF <- rowMeans(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)
    ann.stdevDF <- rowSds(as.matrix(annDF[,3:(n.yr+2)]), na.rm=T)

    outDF <- data.frame(annDF[,c(1:2)], ann.meanDF, ann.stdevDF, n.yr)
    colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "T_n")
    
    
    ### write csv
    write.csv(outDF, "output/T_growth_summary.csv", row.names=F)
    
    
    ### return
    return(outDF)
}
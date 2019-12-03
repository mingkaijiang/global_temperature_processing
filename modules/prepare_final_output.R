prepare_final_output <- function(meanDF, sdDF, nDF, dname.list, return.option) {
    
    ### set colnames for the summaryDFs
    colnames(meanDF) <- c("lon", "lat", paste0("m_", dname.list))
    colnames(sdDF) <- c("lon", "lat", paste0("s_", dname.list))
    colnames(nDF) <- c("lon", "lat", paste0("n_", dname.list))
    
    ### save monthly mean, sd and sample size csv
    write.csv(meanDF, "output/monthly_mean.csv", row.names=F)
    write.csv(sdDF, "output/monthly_sd.csv", row.names=F)
    write.csv(nDF, "output/monthly_sample_size.csv", row.names=F)
    
    
    ### prepare data to calculate overall mean, sds
    mean.matrix <- as.matrix(meanDF[,-c(1:2)])
    sd.matrix <- as.matrix(sdDF[,-c(1:2)])
    n.matrix <- as.matrix(nDF[,-c(1:2)])
    
    ### convert unit from K to C
    mean.matrix <- mean.matrix - 273.15
    
    if (return.option == "growth") {
        ### calculate growth temperature as months when T > 0
        
        ### ignore months with T < 0
        mean.matrix2 <- ifelse(mean.matrix >=0, mean.matrix, "")
        mean.matrix2 <- apply(mean.matrix2, 2, as.numeric)
        
        sd.matrix2 <- ifelse(mean.matrix >=0, sd.matrix, "")
        sd.matrix2 <- apply(sd.matrix2, 2, as.numeric)
        
        n.matrix2 <- ifelse(mean.matrix >=0, n.matrix, "")
        n.matrix2 <- apply(n.matrix2, 2, as.numeric)
        
        ### count number of columns where values are present for each row 
        test <- ifelse(n.matrix2 >= 0, 1, 0)
        test <- apply(test, 2, as.numeric)
        test2 <- rowCounts(test, value = 1, na.rm=T)
        
        ### calculate overall mean
        mean.matrix3 <- mean.matrix2 * n.matrix2
        mean.matrix4 <- rowSums(mean.matrix3, na.rm=T)
        mean.matrix5 <- rowSums(n.matrix2, na.rm=T)
        mean.matrix6 <- mean.matrix4 / mean.matrix5
        
        ### calculate overall sd
        sd.matrix3 <- (n.matrix2 - 1) * (sd.matrix2^2)
        sd.matrix4 <- rowSums(sd.matrix3, na.rm=T)
        sd.matrix5 <- rowSums(n.matrix2, na.rm=T) - test2
        sd.matrix6 <- sqrt(sd.matrix4 / sd.matrix5)
        
        
        ### prepare outputDF
        outDF <- data.frame(meanDF[,c(1:2)], mean.matrix6, sd.matrix6, mean.matrix5)
        colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "T_n")
        
    } else if (return.option == "annual") {
        ### return annual mean T
        
        ### calculate overall mean
        mean.matrix3 <- mean.matrix * n.matrix
        mean.matrix4 <- rowSums(mean.matrix3, na.rm=T)
        mean.matrix5 <- rowSums(n.matrix, na.rm=T)
        mean.matrix6 <- mean.matrix4 / mean.matrix5
        
        ### calculate overall sd
        sd.matrix3 <- (n.matrix - 1) * (sd.matrix^2)
        sd.matrix4 <- rowSums(sd.matrix3, na.rm=T)
        sd.matrix5 <- rowSums(n.matrix, na.rm=T) - dim(n.matrix)[2]
        sd.matrix6 <- sqrt(sd.matrix4 / sd.matrix5)
        
        
        ### prepare outputDF
        outDF <- data.frame(meanDF[,c(1:2)], mean.matrix6, sd.matrix6, mean.matrix5)
        colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "T_n")
    }
    
    
    
    ### write csv
    write.csv(outDF, "output/T_growth_summary.csv", row.names=F)
    
    
    ### return
    return(outDF)
}
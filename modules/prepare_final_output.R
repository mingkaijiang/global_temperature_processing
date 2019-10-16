prepare_final_output <- function(meanDF, sdDF, nDF, dname.list) {
    
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
    
    ### calculate overall mean
    mean.matrix2 <- mean.matrix * n.matrix
    mean.matrix3 <- rowSums(mean.matrix2, na.rm=T)
    mean.matrix4 <- rowSums(n.matrix, na.rm=T)
    mean.matrix5 <- mean.matrix3 / mean.matrix4
    
    ### calculate overall sd
    sd.matrix2 <- (n.matrix - 1) * (sd.matrix^2)
    sd.matrix3 <- rowSums(sd.matrix2)
    sd.matrix4 <- rowSums(n.matrix, na.rm=T) - dim(n.matrix)[2]
    sd.matrix5 <- sqrt(sd.matrix3 / sd.matrix4)
    
    
    ### prepare outputDF
    outDF <- data.frame(meanDF[,c(1:2)], mean.matrix5, sd.matrix5, mean.matrix4)
    colnames(outDF) <- c("lon", "lat", "T_mean", "T_sd", "T_n")
    
    
    ### write csv
    write.csv(outDF, "output/overall_summary.csv", row.names=F)
    
    
    ### return
    return(outDF)
}
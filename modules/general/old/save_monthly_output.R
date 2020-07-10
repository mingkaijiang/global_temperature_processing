save_monthly_output <- function(meanDF, sdDF, nDF, dname.list) {
    ### set colnames for the summaryDFs
    colnames(meanDF) <- c("lon", "lat", paste0("m_", dname.list))
    colnames(sdDF) <- c("lon", "lat", paste0("s_", dname.list))
    colnames(nDF) <- c("lon", "lat", paste0("n_", dname.list))
    
    ### save monthly mean, sd and sample size csv
    write.csv(meanDF, "output/monthly_mean.csv", row.names=F)
    write.csv(sdDF, "output/monthly_sd.csv", row.names=F)
    write.csv(nDF, "output/monthly_n.csv", row.names=F)
    
    
}
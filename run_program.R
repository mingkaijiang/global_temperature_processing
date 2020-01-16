### Generate global gridded temperature profile 

#### source all necessary files
source("prepare.R")


#### set up the storage DF to store the means, sample size and sd,
#### at monthly timestep
meanDF <- create_storage_DF()
sdDF <- create_storage_DF()
nDF <- create_storage_DF()


### create the file name list
dnameDF <- data.frame(rep(c(1979:1979), each=12),
                      rep(c("jan", "feb", "mar", "apr", "may", "jun",
                            "jul", "aug", "sep", "oct", "nov", "dec"), by = 40))
colnames(dnameDF) <- c("year", "month")
dnameDF$yrmonth <- paste(dnameDF$year, dnameDF$month, sep="_")
dname.list <- as.vector(dnameDF$yrmonth)


### call in nc file at monthly timestep,
### calculate monthly mean, sd, and sample size
for (j in 1:length(dname.list)) {
    tmp.out <- prepare_monthly_output(dname=dname.list[j])
    
    ### assign monthly data onto the summary tables
    meanDF[,(j+2)] <- tmp.out[,1]
    sdDF[,(j+2)] <- tmp.out[,2]
    nDF[,(j+2)] <- tmp.out[,3]
}

### calculate mean T, sd T based on all data to get Tgrowth
TgrDF <- prepare_final_output(meanDF, sdDF, nDF, dname.list, return.option="growth")

### calculate Topt
TgrDF$T_opt <- 13.9 + 0.61 * TgrDF$T_mean

### test statistics
TgrDF$stats <- with(TgrDF, (T_opt - T_mean) / T_sd)


### prepare sea surface area mask
ssfDF <- read_sea_surface_mask()

### merge ssf and TgrDF
mgDF <- merge(TgrDF, ssfDF, by=c("lon", "lat"))

### subtract only land
landDF <- mgDF[is.na(mgDF$ssf),]

### prepare global maps for Tgrowth
prepare_map_output(landDF)


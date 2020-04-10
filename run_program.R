############### Generate global gridded temperature profile to investigate
############### the universal parameter
###############
############### code developed by: Mingkai Jiang (m.jiang@westernsydney.edu.au)

#### clear wk space
rm(list=ls(all=TRUE))

#### source all necessary files
source("prepare.R")


###################### General codes
#### set up the storage DF to store the means, sample size and sd,
#### at monthly timestep
meanDF <- create_storage_DF()
sdDF <- create_storage_DF()
nDF <- create_storage_DF()


### create the file name list
dnameDF <- data.frame(rep(c(1979:1984), each=12),
                      rep(c("jan", "feb", "mar", "apr", "may", "jun",
                            "jul", "aug", "sep", "oct", "nov", "dec"), by = 40))
colnames(dnameDF) <- c("year", "month")
dnameDF$yrmonth <- paste(dnameDF$year, dnameDF$month, sep="_")
dname.list <- as.vector(dnameDF$yrmonth)


### call in nc file at monthly timestep,
### calculate monthly temperature mean, sd, and sample size
for (j in 1:length(dname.list)) {
    tmp.out <- prepare_monthly_output(dname=dname.list[j])
    
    ### assign monthly data onto the summary tables
    meanDF[,(j+2)] <- tmp.out[,1]
    sdDF[,(j+2)] <- tmp.out[,2]
    nDF[,(j+2)] <- tmp.out[,3]
}

### prepare annualDF to store data at annual timestep
annDF <- meanDF[,c(1:2)]

### prepare sea surface area mask
ssfDF <- read_sea_surface_mask()


### Notes:
### we have 2 possible ways of calculating Tgrowth:
### A1. Based on annual mean temperature
### A2. Based on months with monthly T > 0 C
### we also have 2 possible ways of calculating Tsd:
### A3. Based on annual mean temperature (i.e. 1 value per year)
### A4. Based on monthly mean temperature (i.e. 12 values per year)

### Do all four and check robustness of the results



#################### Approach 2: Annual mean with monthly mean T > 0 C
### calculate mean T, sd T based on all data to get Tgrowth
TgrDF <- prepare_inter_annual_output(meanDF, sdDF, nDF, annDF, dname.list, 
                                     return.option="growth")

### calculate Topt
TgrDF$T_opt <- 13.9 + 0.61 * TgrDF$T_mean

### prepare a Topt DF for the following approaches
ToptDF1 <- TgrDF[,c("lon", "lat", "T_opt")]

### test statistics
TgrDF$stats <- with(TgrDF, (T_opt - T_mean) / T_sd)


### merge ssf and TgrDF
mgDF <- merge(TgrDF, ssfDF, by=c("lon", "lat"))

### subtract only land
landDF <- mgDF[is.na(mgDF$ssf),]

### prepare global maps, A2 method
### need to go into the function to make the plot
prepare_figure_output_A2(landDF)



#################### Approach 1: annual mean temperature
#### calculate mean T, sd T based on all data to get Tgrowth
#TgrDF <- prepare_inter_annual_output(meanDF, sdDF, nDF, annDF, dname.list, 
#                                     return.option="annual")
#
#### calculate Topt
##TgrDF$T_opt <- 13.9 + 0.61 * TgrDF$T_mean
#TgrDF$T_opt <- ToptDF1$T_opt
#
#### test statistics
#TgrDF$stats <- with(TgrDF, (T_opt - T_mean) / T_sd)
#
#
#### merge ssf and TgrDF
#mgDF <- merge(TgrDF, ssfDF, by=c("lon", "lat"))
#
#### subtract only land
#landDF <- mgDF[is.na(mgDF$ssf),]
#
#### prepare global maps, A1 method
#### need to go into the function to make the plot
#prepare_figure_output_A1(landDF)



#################### Approach 4: Monthly mean with monthly mean T > 0 C
### calculate mean T, sd T based on all data to get Tgrowth
TgrDF <- prepare_intra_annual_output(meanDF, sdDF, nDF, annDF, dname.list, 
                                     return.option="growth")

### calculate Topt
TgrDF$T_opt <- 13.9 + 0.61 * TgrDF$T_mean

### prepare a Topt DF for the following approaches
ToptDF2 <- TgrDF[,c("lon", "lat", "T_opt")]

### test statistics
TgrDF$stats <- with(TgrDF, (T_opt - T_mean) / T_sd)


### merge ssf and TgrDF
mgDF <- merge(TgrDF, ssfDF, by=c("lon", "lat"))

### subtract only land
landDF <- mgDF[is.na(mgDF$ssf),]

### prepare global maps, A4 method
### need to go into the function to make the plot
prepare_figure_output_A4(landDF)


#################### Approach 3: Intra-annual variation with all data
#### calculate mean T, sd T based on all data to get Tgrowth
#TgrDF <- prepare_intra_annual_output(meanDF, sdDF, nDF, annDF, dname.list, 
#                                     return.option="annual")
#
#### calculate Topt
##TgrDF$T_opt <- 13.9 + 0.61 * TgrDF$T_mean
#TgrDF$T_opt <- ToptDF2$T_opt
#
#
#### test statistics
#TgrDF$stats <- with(TgrDF, (T_opt - T_mean) / T_sd)
#
#
#### merge ssf and TgrDF
#mgDF <- merge(TgrDF, ssfDF, by=c("lon", "lat"))
#
#### subtract only land
#landDF <- mgDF[is.na(mgDF$ssf),]
#
#### prepare global maps, A3 method
#### need to go into the function to make the plot
#prepare_figure_output_A3(landDF)



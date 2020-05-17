############### Generate global gridded temperature profile to investigate
############### the universal parameter
###############
############### code developed by: Mingkai Jiang (m.jiang@westernsydney.edu.au)

######################################################################################
################################## General codes #####################################
#### clear wk space
rm(list=ls(all=TRUE))

#### source all necessary files
source("prepare.R")
################################## End general codes #####################################
##########################################################################################



###########################################################################################
################## Basic code to process temperature data #################################
#### Structure:
#### 1. prepare storage DF
#### 2. loop through custom-defined year, 
####    and perform monthly mean, sd and n calculation, based on 6-hourly data
#### 3. prepare annual storage DF
#### 4. prepare sea surface mask

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

### save mean, sd and n datasets
save_monthly_output(meanDF, sdDF, nDF, dname.list)

######################################## End basic code ##################################
##########################################################################################


##########################################################################################
############################# temperature calculations ###################################
### Structure:
### 1.1. Use monthly mean, sd and n information to calculate pooled mean Tgrowth and pooled sd.
###      The pooled sd is indicative of diurnal variability.
###      Tgrowth is monthly mean T (i.e. no filtering assumption).
### 1.2. Use monthly mean, sd and n information to calculate pooled mean Tgrowth and pooled sd.
###      The pooled sd is indicative of diurnal variability.
###      Tgrowth is an average of monthly mean T > 0 degree C.
###      Same rule applies to sd calculation. 
###
### 2.1. Use monthly mean to calculate pooled mean Tgrowth, 
###      but calculate sd based on the monthly mean T.
###      SD is indicative of seasonal interannual variability.
###      Tgrowth is monthly mean T (i.e. no filtering assumption).
### 2.2. Use monthly mean to calculate pooled mean Tgrowth, 
###      but calculate sd based on the monthly mean T.
###      SD is indicative of seasonal interannual variability.
###      Tgrowth is an average of monthly mean T > 0 degree C.
###      Same rule applies to sd calculation. 
###
### 3.1. Use monthly mean to calculate annual mean Tgrowth,
###      and use annual mean to obtain SD. 
###      SD is indicative of inter-annual variability.
###      Based on all data within the year (i.e. 12 months).
### 3.2. Use monthly mean where Tmean > 0 degree C to calculate annual mean Tgrowth,
###      and use annual mean to obtain SD.
###      SD is indicative of inter-annual variability.

###########
#### 1.1. pooled monthly mean Tmean, pooled SD of monthly values
###       SD is indicative of diurnal and day-to-day variability within each month. 
###       Use all data within a year (i.e. do not filter out monthly mean < 0 degree C)
landDF1 <- prepare_diurnal_output(meanDF, sdDF, nDF, 
                                  annDF, ssfDF, dname.list, 
                                  return.option="annual")

#### 1.2. same as 1.1, but with growing season Tmean (i.e. Tmean > 0 degree C)
landDF2 <- prepare_diurnal_output(meanDF, sdDF, nDF, 
                                  annDF, ssfDF, dname.list, 
                                  return.option="growth")

### need to go into the function to make the plot
## landDF1
plotDF = landDF1
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_annual"
prepare_figure_output_diurnal(plotDF = landDF1,
                              sd.filter.option = "no.filter",
                              outdir = "output/diurnal/", 
                              outname = "diurnal_annual")


## landDF2
plotDF = landDF2
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_growth"
prepare_figure_output_diurnal(plotDF = landDF2,
                              sd.filter.option = "no.filter",
                              outdir = "output/diurnal/", 
                              outname = "diurnal_growth")

###########
#### 2.1. pooled monthly mean Tmean, 
###       SD based on monthly Tmean
###       SD is indicative of seasonal interannual variability. 
###       Use all data within a year (i.e. do not filter out monthly mean < 0 degree C)
landDF3 <- prepare_intra_annual_output(meanDF, 
                                       annDF, ssfDF, dname.list, 
                                       return.option="annual")

#### 2.2. same as 2.1, but with growting season Tmean (i.e. monthly mean T > 0 degree C)
landDF4 <- prepare_intra_annual_output(meanDF, 
                                       annDF, ssfDF, dname.list, 
                                       return.option="growth")

### need to go into the function to make the plot
prepare_figure_output_intraannual(plotDF = landDF3,
                                  sd.filter.option = "no.filter",
                                  outdir = "output/diurnal/", 
                                  outname = "diurnal")


###########
#### 3.1. calculate annual Tmean based on monthly Tmean, 
###       SD based on annual Tmean
###       SD is indicative of interannual variability. 
###       Use all data within a year (i.e. do not filter out monthly mean < 0 degree C)
landDF5 <- prepare_inter_annual_output(meanDF, 
                                       annDF, ssfDF, dname.list, 
                                       return.option="annual")

#### 3.2. same as 3.1, but with growing season Tmean 
###       (i.e. calculate annual Tmean based on monthly mean > 0 degree C)
landDF6 <- prepare_inter_annual_output(meanDF, 
                                       annDF, ssfDF, dname.list, 
                                       return.option="growth")


#### prepare global maps, A2 method
#### need to go into the function to make the plot
#prepare_figure_output_A2(landDF)





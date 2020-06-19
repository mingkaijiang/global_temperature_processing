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
dnameDF <- data.frame(rep(c(1979:2018), each=12),
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

#### 1.3. Making plots
### need to go into the function to make the plot
## landDF1
plotDF = landDF1
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_annual"
prepare_figure_output(plotDF = landDF1,
                      sd.filter.option = "no.filter",
                      outdir = "output/diurnal/", 
                      outname = "diurnal_annual")

## landDF2
plotDF = landDF2
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_growth"
prepare_figure_output(plotDF = landDF2,
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

#### 2.3. Making plots 
### need to go into the function to make the plot
## plotDF3
plotDF = landDF3
sd.filter.option = "no.filter"
outdir = "output/intraannual/"
outname = "intraannual_annual"
prepare_figure_output(plotDF = landDF3,
                      sd.filter.option = "no.filter",
                      outdir = "output/intraannual/", 
                      outname = "intraannual_annual")

## plotDF4
plotDF = landDF4
sd.filter.option = "no.filter"
outdir = "output/intraannual/"
outname = "intraannual_growth"
prepare_figure_output(plotDF = landDF4,
                      sd.filter.option = "no.filter",
                      outdir = "output/intraannual/", 
                      outname = "intraannual_growth")

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


#### 3.3. Making plots 
### need to go into the function to make the plot
## plotDF5
plotDF = landDF5
sd.filter.option = "no.filter"
outdir = "output/interannual/"
outname = "interannual_annual"
prepare_figure_output(plotDF = landDF5,
                      sd.filter.option = "no.filter",
                      outdir = "output/interannual/", 
                      outname = "interannual_annual")

## plotDF6
plotDF = landDF6
sd.filter.option = "no.filter"
outdir = "output/interannual/"
outname = "interannual_growth"
prepare_figure_output(plotDF = landDF6,
                      sd.filter.option = "no.filter",
                      outdir = "output/interannual/", 
                      outname = "interannual_growth")



############################# end temperature calculations ###############################
##########################################################################################



##########################################################################################
############################# biome-specific patterns ####################################
#### structure:
#### 1.1. Merge DF with biome grids
#### 1.2. Make biome-specific plots

## landDF1
plotDF = landDF1
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_annual"
merge_biome_information_and_plot(plotDF = landDF1,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/diurnal/", 
                                 outname = "diurnal_annual")

## landDF2
plotDF = landDF2
sd.filter.option = "no.filter"
outdir = "output/diurnal/"
outname = "diurnal_growth"
merge_biome_information_and_plot(plotDF = landDF2,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/diurnal/", 
                                 outname = "diurnal_growth")



## plotDF3
plotDF = landDF3
sd.filter.option = "no.filter"
outdir = "output/intraannual/"
outname = "intraannual_annual"
merge_biome_information_and_plot(plotDF = landDF3,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/intraannual/", 
                                 outname = "intraannual_annual")

## plotDF4
plotDF = landDF4
sd.filter.option = "no.filter"
outdir = "output/intraannual/"
outname = "intraannual_growth"
merge_biome_information_and_plot(plotDF = landDF4,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/intraannual/", 
                                 outname = "intraannual_growth")

## plotDF5
plotDF = landDF5
sd.filter.option = "no.filter"
outdir = "output/interannual/"
outname = "interannual_annual"
merge_biome_information_and_plot(plotDF = landDF5,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/interannual/", 
                                 outname = "interannual_annual")

## plotDF6
plotDF = landDF6
sd.filter.option = "no.filter"
outdir = "output/interannual/"
outname = "interannual_growth"
merge_biome_information_and_plot(plotDF = landDF6,
                                 sd.filter.option = "no.filter",
                                 outdir = "output/interannual/", 
                                 outname = "interannual_growth")


############################## end biome-specific patterns ###############################
##########################################################################################


##########################################################################################
############################## Implications - climate change #############################
#### Placeholder for code to investigate climate change effect on the universal number

### Downloaded CMIP5 ACCESS1.0 r1i1p1 RCP.5 daily data 
### in .nc format
### convert data into format compatible with the above analysis
### then looked at how the intra- and inter-annual universal number change




######################## end implications - climate change ###############################
##########################################################################################

##########################################################################################
############################## Implications - plant traits ###############################
#### Placeholder for code to investigate the implication of the universal number on plant traits
#### Or simply their correlations

### note that, when downloading data, you need to manually deleted unwanted variables
### the WGET script generates downloading script for all possible variables
### whereas we are only interested in tas variable
### also, need to generate script to process data from each model
### as different model has different structure and variable name

### the WGET script needs to use command
### chmod +x xx.sh first
### then run with ./.sh

process_cmip5_HadGEM2AO_rcp85_data(sourceDir="data", 
                                   destDir="output/CMIP5")

RCP85_HadGEM2AO <- prepare_inter_annual_output_rcp85_HadGEM2AO(sourceDir="output/CMIP5", 
                                                               destDir="output/CMIP5/interannual", 
                                                               return.option="growth")

    
plotDF = RCP85_HadGEM2AO
sd.filter.option = "no.filter"
outdir = "output/CMIP5/interannual/"
outname = "RCP85_HadGEM2AO_interannual_growth"    
prepare_figure_output(plotDF = RCP85_HadGEM2AO,
                      sd.filter.option = "no.filter",
                      outdir = "output/CMIP5/interannual/", 
                      outname = "RCP85_HadGEM2AO_interannual_growth")

########################## end implications - plant traits ###############################
##########################################################################################


### To do list:
### 2. investigate if precipitation can explain the biome-specific statistical comparison
###    i.e. linear model, with biome and precipitation as fixed factor
### 3. write result interpretation
### 4. add background information to introduction
### 5. add to-analyze list for future investigations to enrich the paper
###    possible list include:
###                          5.1. CMIP6 temperature to see if this universal number change
###                          5.2. Implication of this number in TBMs
###                          5.3. How it correlates with plant's thermal adaptation strategy/traits
###                          5.4. Why it's worth Nature publishing
 



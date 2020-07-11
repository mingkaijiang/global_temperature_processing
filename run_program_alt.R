############### Generate global gridded temperature profile to investigate
############### the universal parameter for thermal safety margin 
############### for optimal plant photosynthesis
###############
############### code developed by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### Structure:
####           1. General codes to prepare the repository
####           2. General code to prepare ERA interim temperature dataset
####           3. Different ways to compute Tgrowth and Tsd
####           4. Plotting high frequency and low frequency results 
####              (i.e. diurnal, seasonal and inter-annual),
####              and biome plots
####           5. Investigate CMIP5 future scenarios

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

### 1. create a date DF 
### run once and it's done
create_dayDF(sourceDir="/Volumes/TOSHIBAEXT/era_interim/raw/",
             destDir="output")


### get lonlat information
get_lonlatDF(sourceDir="/Volumes/TOSHIBAEXT/era_interim/raw/",
             destDir="output")

### 2. convert ERA dataset from per month per file to per region per file
prepare_ERA_INTERIM_dataset_and_split(sourceDir="/Volumes/TOSHIBAEXT/era_interim/raw/",
                                      destDir="/Volumes/TOSHIBAEXT/era_interim/processed/")


### 3. calculate Tmean, Topt and Tsd for each grids, with different options
calculate_gridded_temperature_parameters(sourceDir="/Volumes/TOSHIBAEXT/era_interim/processed/", 
                                         destDir="output/splitted/",
                                         return.option="annual")

### 4. Merge the caluclated temperature data, then remove sea surface area mask
landDF <- merge_splitted_files_and_remove_sea_surface(sourceDir="output/splitted/", 
                                                      destDir="output/merged/",
                                                      return.option="annual")

### 5. plot
plotDF = landDF
sd.filter.option = "no.filter"
outdir = "output/alternative/"
outname = "diurnal_annual"
prepare_figure_output(plotDF = landDF1,
                      sd.filter.option = "no.filter",
                      outdir = "output/alternative/", 
                      outname = "diurnal_annual")

### it seems that there is a bug somehere, because the global Tmean is not as expected. 
### possible source of error is where we merge the files, or calculate Tmean

######################################## End basic code ##################################
##########################################################################################



############################# end temperature calculations ###############################
##########################################################################################


 



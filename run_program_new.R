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
                      outdir = "output/diurnal/", 
                      outname = "diurnal_annual")

######################################## End basic code ##################################
##########################################################################################



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
### 1. clean code on historic calculation to speed up the process, 
###    abd to calculate Tgrowth based on all data at 6-hourly timestep.
### 2. Do the same for the RCP model.
### 3. Add diurnal and seasonal calculation for the RCP model.
### 4. Download more RCP model and write their individual code.
### 5. Write result interpretation.
### 6. Make figures according to journal/storyline requirements

 



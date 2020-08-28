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
#### 2. convert data from per month to per region
#### 3. calculate Tmean, Tsd based on defined period (hourly, daily, monthly, or annual)
#### 4. merge all data
#### 5. Plotting

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
### return options: hourly - based on 6 hourly data to calculate mean and sd
###                 daily - based on daily mean to calculate mean and sd
###                 monthly - based on monthly mean to caclulate mean and sd
###                 annual - based on annual mean to calculate mean and sd
### for now, don't split into all data and growth period data, but will need to come back!
calculate_gridded_temperature_parameters(sourceDir="/Volumes/TOSHIBAEXT/era_interim/processed/", 
                                         destDir="output/splitted/hourly/",
                                         return.option="hourly")

calculate_gridded_temperature_parameters(sourceDir="/Volumes/TOSHIBAEXT/era_interim/processed/", 
                                         destDir="output/splitted/daily/",
                                         return.option="daily")

calculate_gridded_temperature_parameters(sourceDir="/Volumes/TOSHIBAEXT/era_interim/processed/", 
                                         destDir="output/splitted/monthly/",
                                         return.option="monthly")

calculate_gridded_temperature_parameters(sourceDir="/Volumes/TOSHIBAEXT/era_interim/processed/", 
                                         destDir="output/splitted/annual/",
                                         return.option="annual")



### 4. Merge the caluclated temperature data, then remove sea surface area mask
landDF1 <- merge_splitted_files_and_remove_sea_surface(sourceDir="output/splitted/hourly/", 
                                                       destDir="output/merged/hourly/",
                                                       return.option="hourly")

landDF2 <- merge_splitted_files_and_remove_sea_surface(sourceDir="output/splitted/daily/", 
                                                       destDir="output/merged/daily/",
                                                       return.option="daily")

landDF3 <- merge_splitted_files_and_remove_sea_surface(sourceDir="output/splitted/monthly/", 
                                                       destDir="output/merged/monthly/",
                                                       return.option="monthly")

landDF4 <- merge_splitted_files_and_remove_sea_surface(sourceDir="output/splitted/annual/", 
                                                       destDir="output/merged/annual/",
                                                       return.option="annual")

### 5. plot
## hourly
plotDF = landDF1
sd.filter.option = "no.filter"
outdir = "output/alternative/hourly/"
outname = "hourly_all"
prepare_figure_output(plotDF = landDF1,
                      sd.filter.option = "no.filter",
                      outdir = "output/alternative/hourly/", 
                      outname = "hourly_all")

make_paper_quality_figures(plotDF = landDF1,
                           sd.filter.option = "no.filter",
                           outdir = "output/alternative/hourly/", 
                           outname = "hourly_all")


make_paper_quality_figures_ecosystem(plotDF = landDF1,
                                     sd.filter.option = "no.filter",
                                     outdir = "output/alternative/hourly/", 
                                     outname = "hourly_all")


## daily
plotDF = landDF2
sd.filter.option = "no.filter"
outdir = "output/alternative/daily/"
outname = "daily_all"
prepare_figure_output(plotDF = landDF2,
                      sd.filter.option = "no.filter",
                      outdir = "output/alternative/daily/", 
                      outname = "daily_all")

make_paper_quality_figures(plotDF = landDF2,
                           sd.filter.option = "no.filter",
                           outdir = "output/alternative/daily/", 
                           outname = "daily_all")


make_paper_quality_figures_ecosystem(plotDF = landDF2,
                                     sd.filter.option = "no.filter",
                                     outdir = "output/alternative/daily/", 
                                     outname = "daily_all")


## monthly
plotDF = landDF3
sd.filter.option = "no.filter"
outdir = "output/alternative/monthly/"
outname = "monthly_all"
prepare_figure_output(plotDF = landDF3,
                      sd.filter.option = "no.filter",
                      outdir = "output/alternative/monthly/", 
                      outname = "monthly_all")

make_paper_quality_figures(plotDF = landDF3,
                           sd.filter.option = "no.filter",
                           outdir = "output/alternative/monthly/", 
                           outname = "monthly_all")


make_paper_quality_figures_ecosystem(plotDF = landDF3,
                                     sd.filter.option = "no.filter",
                                     outdir = "output/alternative/monthly/", 
                                     outname = "monthly_all")


## annual
plotDF = landDF4
sd.filter.option = "no.filter"
outdir = "output/alternative/annual/"
outname = "annual_all"
prepare_figure_output(plotDF = landDF4,
                      sd.filter.option = "no.filter",
                      outdir = "output/alternative/annual/", 
                      outname = "annual_all")

make_paper_quality_figures(plotDF = landDF4,
                           sd.filter.option = "no.filter",
                           outdir = "output/alternative/annual/", 
                           outname = "annual_all")


make_paper_quality_figures_ecosystem(plotDF = landDF4,
                                     sd.filter.option = "no.filter",
                                     outdir = "output/alternative/annual/", 
                                     outname = "annual_all")

######################################## End basic code ##################################
##########################################################################################



############################# end temperature calculations ###############################
##########################################################################################


 



#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

#if(!dir.exists("output/diurnal")) {
#    dir.create("output/diurnal", showWarnings = FALSE)
#}
#
#if(!dir.exists("output/intraannual")) {
#    dir.create("output/intraannual", showWarnings = FALSE)
#}
#
#if(!dir.exists("output/interannual")) {
#    dir.create("output/interannual", showWarnings = FALSE)
#}
#
#if(!dir.exists("output/CMIP5")) {
#    dir.create("output/CMIP5", showWarnings = FALSE)
#}

#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(raster,
               ncdf4,
               spatstat,
               lattice,
               fields,
               matrixStats,
               RColorBrewer,
               ggplot2,
               cowplot,
               doBy,
               sf,
               rgdal,
               maptools,
               plyr,
               raster,
               ggthemes,
               reshape2,
               lubridate)    


#### Sourcing all R files in the modules subdirectory
sourcefiles1 <- dir("modules/general", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles1)source(z)

sourcefiles2 <- dir("modules/diurnal", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles2)source(z)

sourcefiles3 <- dir("modules/intraannual", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles3)source(z)

sourcefiles4 <- dir("modules/interannual", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles4)source(z)



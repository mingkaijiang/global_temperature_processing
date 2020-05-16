#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}

if(!dir.exists("output/hourly")) {
    dir.create("output/hourly", showWarnings = FALSE)
}

if(!dir.exists("output/intraannual")) {
    dir.create("output/intraannual", showWarnings = FALSE)
}

if(!dir.exists("output/interannual")) {
    dir.create("output/interannual", showWarnings = FALSE)
}

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
               cowplot)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


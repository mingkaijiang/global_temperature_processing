#### Create output folder
if(!dir.exists("output")) {
    dir.create("output", showWarnings = FALSE)
}


#### Install packages
if(!require(pacman))install.packages("pacman")
pacman::p_load(raster,
               ncdf4,
               spatstat,
               lattice,
               fields)    


#### Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


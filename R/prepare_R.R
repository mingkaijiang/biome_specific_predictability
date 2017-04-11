#### Prepare folder structure and libraries
####
#### 
################################################################################

#### Install libraries
if(!require(pacman))install.packages("pacman")
pacman::p_load(ncdf4,
               #chron,
               #RColorBrewer,
               #lattice,
               #plyr,
               #data.table,
               #reshape2,
               #maps,
               #changepoint,
               #SDMTools,
               #sm,
               #fields,
               #spatstat,
               #vioplot,
               #aplpack,
               #plyr,
               #ggplot2,
               raster) # add other packages needed to this list


#### Sourcing all R files in the function subdirectory
sourcefiles <- dir("Functions", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)
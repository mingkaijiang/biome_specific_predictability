#### Prepare folder structure and libraries
####
#### 
################################################################################

#### Install libraries
if(!require(pacman))install.packages("pacman")
pacman::p_load(ncdf4,
               data.table,
               #chron,
               #RColorBrewer,
               #lattice,
               #plyr,
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


#

#### Sourcing all R files in the function subdirectory
sourcefiles <- dir("Functions", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


#### Create analyses folders if not exist
if(!dir.exists("/analyses")) {
    dir.create(paste0(getwd(), "/analyses"), showWarnings = FALSE)
}

#### Create plot folders if not exist
if(!dir.exists("/Plots")) {
    dir.create(paste0(getwd(), "/Plots"), showWarnings = FALSE)
}

#### Create Tables folders if not exist
if(!dir.exists("/Tables")) {
    dir.create(paste0(getwd(), "/Tables"), showWarnings = FALSE)
}

#### Create data folders if not exist
if(!dir.exists("/data")) {
    dir.create(paste0(getwd(), "/data"), showWarnings = FALSE)
}

#### store working directory information
cwd <- getwd()

#### store data path
ncDir <- paste0(cwd, "/data/raw_data")
dataDir <- paste0(cwd, "/data")
plotDir <- paste0(cwd, "/Plots")
tableDir <- paste0(cwd, "/Tables")
analysesDir <- paste0(cwd, "/analyses")

#### Create the necessary sub-folders
dir.create(ncDir, showWarnings = FALSE)


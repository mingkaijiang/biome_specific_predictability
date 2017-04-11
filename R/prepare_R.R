#### Prepare folder structure and libraries
####
#### 
################################################################################

######################## Libraries ##################################
#### Install libraries
if(!require(pacman))install.packages("pacman")
pacman::p_load(ncdf4,
               data.table,
               psych,
               fmsb,
               ks,
               vegan,
               #chron,
               #RColorBrewer,
               lattice,
               gridExtra,
               hexbin,
               #plyr,
               #reshape2,
               #maps,
               #changepoint,
               #SDMTools,
               sm,
               fields,
               #spatstat,
               vioplot,
               aplpack,
               #plyr,
               #ggplot2,
               raster) # add other packages needed to this list

##################### Function sourcing ###########################
#### Sourcing all R files in the function subdirectory
sourcefiles <- dir("Functions", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)


##################### Create Folders #############################
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

################# Directory settings ###########################
#### store working directory information
cwd <- getwd()

#### store data path
ncDir <- paste0(cwd, "/data/raw_data")
dataDir <- paste0(cwd, "/data")
plotDir <- paste0(cwd, "/Plots")
tableDir <- paste0(cwd, "/Tables")
analysesDir <- paste0(cwd, "/analyses")
corDir <- paste0(cwd, "/data/cru_biome_data")

#### Create the necessary sub-folders
dir.create(ncDir, showWarnings = FALSE)

################# Graphic settings ###########################
# save default par()
opar <- par()

# suppression warning messages globally
options(warn=-1)

# prepare legend labels
temp.lab <- c("<-4.7", "-2.1", "0.5", "3.0", "5.6", "8.2",
              "10.8", "13.4", "16.0", "18.5", "21.1", ">21.1")
prec.lab <- c("0", "2.3", "5.3", "12.2", "28", "64",
              "148", "340", "783", "1801", "4142",
              ">4142")

# prepare color list
color.list <- c("#882E72", "#B178A6", "#D6C1DE", 
                "#1965B0", "#5289C7", "#7BAFDE", 
                "#4EB265", "#90C987", "#CAE0AB", 
                "#F7EE55", "#F6C141", "#F1932D", 
                "#E8601C", "#DC050C")


################# Biome settings ###########################
# prepare biome name abbrev. list
biomeN <- c("1. Trop m broadleaf",            ##1
            "2. Trop d broadleaf",            ##2
            "3. Trop conifer",                ##3
            "4. Temp broadleaf & mixed",      ##4
            "5. Temp conifer",                ##5
            "6. Boreal forests",              ##6
            "7. Trop grass",                  ##7
            "8. Temp grass",                  ##8
            "9. Flooded grass",               ##9
            "10. Montane grass",               ##10
            "11. Tundra",                      ##11
            "12. Medit forest",                ##12
            "13. Desert",                      ##13
            "14. Mangroves")                   ##14

# prepare biome name abbrev. list
biome <- c("Trop m broadleaf",            ##1
           "Trop d broadleaf",            ##2
           "Trop conifer",                ##3
           "Temp broadleaf & mixed",      ##4
           "Temp conifer",                ##5
           "Boreal forests",              ##6
           "Trop grass",                  ##7
           "Temp grass",                  ##8
           "Flooded grass",               ##9
           "Montane grass",               ##10
           "Tundra",                      ##11
           "Medit forest",                ##12
           "Desert",                      ##13
           "Mangroves")                   ##14

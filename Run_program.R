#### Main program 
#### Purpose: 
#### 1. compute temperature and precipitation predictability based on CRU dataset
#### 2. compare mean annual climate statistics and those of the predictability
#### 3. Assign biome information and constrast spatial differences
#### 4. Conduct case studies to differentiate the biome-specific climate patterns
#### 5. perform statistical analyses to identify the role of predictability 
#### 6. Make summary tables and manuscript plots
####
#### Author: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 

####################################################################################
#### Make sure everything is clear
rm(list=ls(all=TRUE))

#### prepare wk spaces and libraries
source("R/prepare_R.R")

####################################################################################
#### assume you have downloaded the data as registration is required to download the data
#### Download the data into "/data/raw_data", then run the following codes to process them

####################################################################################
#### Preliminary processing CRU climate data  - skipping step 1 - 3 as they take very long to run!

#### Step 1. convert from nc to csv file format
## CRU temperature
#nc_to_csv(inFile=paste0(ncDir, "/cru_ts3.21.1901.2012.tmp.dat.nc"),
#          outFile=paste0(dataDir,"/temp_DF.csv"))
#
## CRU precipitation
#nc_to_csv(inFile=paste0(ncDir, "/cru_ts3.21.1901.2012.pre.dat.nc"),
#          outFile=paste0(dataDir,"/prec_DF.csv"))
#
#
#### Step 2. remove duplicated data entries  -- Note: may not needed.
## temperature
#remove_duplicate(inFile=paste0(dataDir,"/temp_DF.csv"),
#                 outFile=paste0(dataDir,"/temp_DF_processed.csv"))
#
## precipitation
#remove_duplicate(inFile=paste0(dataDir,"/prec_DF.csv"),
#                 outFile=paste0(dataDir,"/prec_DF_processed.csv"))


### Step 3. calculate predictability using CRU climate data
# temperature
PCM_temp(sourceDir = dataDir, destDir = dataDir)

# precipitation
PCM_prec(sourceDir = dataDir, destDir = dataDir)

### Step 4. Calculate climate annual mean and annual sums
# temperature
tempMeans(inFile=paste0(dataDir, "/temp_DF.csv"),
          outFile=paste0(dataDir, "/temp_DF_annual_mean.csv"))

# precipitation
precMeanSums(inFile=paste0(dataDir, "/prec_DF.csv"),
             outFile=paste0(dataDir, "/prec_DF_annual_sum.csv"))

### Step 5. Project BIOME onto PCM file
biomeProject(corFile=paste0(corDir, "/CRU_Biome.csv"),    # where does this come from?
             tempFile=paste0(dataDir, "/temp_PCM.csv"),
             precFile=paste0(dataDir, "/pre_PCM.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM.csv"))

### Step 6. Save PCM with prec means and sums and temp means
match_climate(tempFile=paste0(dataDir, "/temp_DF_annual_mean.csv"),
             precFile=paste0(dataDir, "/pre_DF_annual_sum.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM.csv"),
             fullFile=paste0(dataDir, "/biome_temp_prec_full.csv"))

####################################################################################
################# Main program for plotting ########################################

# read in full dataset (gridded temperature means, precipitation means and sums, temp and prec PCM)
myDF <- read.table(paste(destDir, "/biome_temp_prec_full.csv", sep=""), 
                   header=T,sep=",")

colnames(myDF) <- c("CRU_Site", "lon", "lat", "tempP",
                    "tempC", "tempM", "BIOME", "REALM", "precP",
                    "precC", "precM", "temp", "prec_sum", "prec_mean")


# Categorize temp and prec climate
myDF$temp_group <- round_any(myDF$temp, 10, f=ceiling)
myDF$prec_group <- round_any(myDF$prec_sum, 1000, f=ceiling)

# generate temp and prec classes
newDF <- classPrep(myDF)

# Calculate Ie for temp and prec
plotDF <- iefactor(newDF)

# Calculate summary df
summary <- summaryPrep(plotDF)

write.table(summary, paste(getwd(), "/summary_statistics.csv", sep=""),
            col.names=T,row.names=F, sep=",")

# Calculate summary df
minDF <- summaryPrep_min(plotDF)
maxDF <- summaryPrep_max(plotDF)

################# Graphic settings ################################################
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

####################################################################################
# Plot biome distribution
pdf(paste(getwd(), "/image/biomePLOT.pdf", sep=""),
    width=10, height=8)
biomePlot(plotDF)
dev.off()

####################################################################################
### Summary statistics

## plot 2d with two directional error bars
pdf(paste(getwd(), "/Summary_2_d.pdf", sep=""),
    width = 10, height = 8)
summary2d(summary)
dev.off()

## Plot radar plots for summary data
pdf(paste(getwd(), "/radar_plot.pdf", sep=""))
radar_summary(summary)
dev.off()

pdf(paste(getwd(), "/image/summary_2_d.pdf", sep=""),
    width = 10, height = 8)
summary2dimage(summary)
dev.off()

## Plot radar plots for summary data
pdf(paste(getwd(), "/image/radar_plot.pdf", sep=""),
    width = 8, height = 12)
radar_summary_image2(summary)
dev.off()

## Plot star plots for summary data
pdf(paste(getwd(), "/image/star_plot.pdf", sep=""))
star_summary_image(summary)
dev.off()


####################################################################################
### Gridded maps

# create pdf file of global maps
pdf(paste(getwd(), "/global_maps_of_climate.pdf", sep=""),
    width = 10, height = 8)
PlotMaps(plotDF)
dev.off()


# create pdf file of global maps
pdf(paste(getwd(), "/image/global_maps_of_PCM.pdf", sep=""),
    width = 10, height = 9)
PlotPCMMaps(plotDF)
dev.off()

# create pdf file of global maps
pdf(paste(getwd(), "/image/global_maps_of_annual_climate.pdf", sep=""),
    width = 8, height = 6)
PlotMapsAnnual(plotDF)
dev.off()

####################################################################################
## kernel density plot
pdf(paste(getwd(), "/kernel_density.pdf", sep=""))
kernel_multi(plotDF)
dev.off()

####################################################################################
## biome specific Ie factor plot
pdf(paste(getwd(), "/biome_Ie.pdf", sep=""))
Ieplot(plotDF)
dev.off()

####################################################################################
# Plot all data 2-d temperature vs. precipitation
# and temperature P vs. precipitation P graphs
pdf(paste(getwd(), "Biome_all_normal.pdf", sep="/"))
biomeAll(plotDF)
dev.off()

# Plot biome specific 2-d temperature vs. precipitation, 
# and temperature predictability vs. precipitation predictability graphs
pdf(paste(getwd(), "Biome_bagplot_normal.pdf", sep="/"),
    width = 22, height = 26)
biomeBagPlot(plotDF)
dev.off()

# Plot biome specific 2-d temperature vs. precipitation, 
# and temperature predictability vs. precipitation predictability density graphs
pdf(paste(getwd(), "Biome_density_normal.pdf", sep="/"),
    width = 22, height = 26)
biomeDensityPlot(plotDF)
dev.off()

####################################################################################
## PCA analysis for each biome using summary statistics
pdf(paste(getwd(), "/PCA_summary.pdf", sep=""))
SummaryPCA(summary)
dev.off()

pdf(paste(getwd(), "/image/PCA_summary.pdf", sep=""))
SummaryPCA_image(summary)
dev.off()

####################################################################################
## PCA analysis for each biome using all data
pdf(paste(getwd(), "/PCA_analysis.pdf", sep=""))

## PCA_summary table includes:
## 1. Biome specific PC1 and PC2 variance explained
## 2. Biome specific correlation coefficients between PC and climate variables
PCA_summary<-BiomePCA(plotDF)
dev.off()

####################################################################################
## PCA analysis for all data, and output PC12 onto CRU grids
pdf(paste(getwd(), "/PCA_all_analysis.pdf", sep=""))

## PCA_summary table includes:
cruPCA<-TotalPCA(plotDF)
dev.off()

write.table(cruPCA, paste(destDir, "/CRU_with_PCA.csv", sep=""),
            col.names=T,row.names=F,sep=",")

####################################################################################
## Prepare subset of data include 50th percentile closest to mean of:
## 1. temp vs. prec
## 2. temp P vs. prec P
## 3. temp C vs. prec C
## 4. temp M vs. prec M

abs_sub <- Percentile50_absolute(plotDF)
p_sub <- Percentile50_p(plotDF)
c_sub <- Percentile50_c(plotDF)
m_sub <- Percentile50_m(plotDF)

####################################################################################
## Plot 50th percentile data
pdf(paste(getwd(), "Biome_bagplot_50.pdf", sep="/"),
    width = 22, height = 26)
plot50th(abs_sub, p_sub, plotDF)
dev.off()

####################################################################################
## Plot spatially the 50th percentile data
pdf(paste(getwd(), "/Spatial_50th.pdf", sep=""),
    width = 28, height = 16)
spatial50(abs_sub, p_sub)
dev.off()

####################################################################################
## Project NDVI data onto CRU dataframe
outPath <- paste(getwd(), "/CRU_NDVI_gridded.csv", sep="")
ndvi_to_cru(plotDF, outPath)

newDF <- read.table(paste(getwd(), "/CRU_NDVI_gridded.csv", sep=""),
                    header=T,sep=",")

## Statistical tests on NDVI
pdf(paste(getwd(), "/NDVI_statistical_tests.pdf", sep=""),
    width = 10, height = 8)
ndvi_tests(newDF)
dev.off()

####################################################################################
## Process species richness data and save onto CRU dataframe

inFile <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/SpeciesRichness/extract"
outFile <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/SpeciesRichness/species_richness_cru.csv"

richnessCRU(inFile, outFile)

spDF <- read.table(outFile, header=T,sep=",")
####################################################################################
## Correlate species richness with CRU climates

ndviDF <- read.table(paste(getwd(), "/CRU_NDVI_gridded.csv", sep=""),
                     header=T,sep=",")

pdf(paste(getwd(), "/bird_richness_statistical_tests.pdf", sep=""),
    width = 12, height = 8)
species_analysis(spDF, plotDF, ndviDF) 
dev.off()

####################################################################################
## Process USDA FIA dataset
require(data.table)
inFile <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/USDA_FID"
predFile <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU"

## read in file
cor <- fread(paste(inFile, "/PLOTGEOM.csv", sep=""),
             header=T,sep=",")
fid <- fread(paste(inFile, "/TREE.csv", sep=""),
             header=T,sep=",")
predDF <- read.table(paste(predFile, "/biome_temp_prec_full.csv", sep=""), 
                     header=T,sep=",")

# Compute all species self-thinning rule, for information only
#fia_st_all(cor, fid)

# Compute species group level self-thinning rule
species_model <- fia_st_species(fid)
write.table(species_model, paste(inFile, "/species_model.csv",sep=""),
            col.names=T,row.names=F,sep=",")

# Compute plot level self-thinning statistics, then save onto CRU dataframe
plot_statistics <- fia_st_plot(cor, fid)
write.table(plot_statistics, paste(inFile, "/plot_model_summary.csv",sep=""),
            col.names=T,row.names=F,sep=",")

####################################################################################
## Project CRU climate onto fia exponent dataset
fiaDF <- read.table(paste(inFile, "/plot_model_summary.csv",sep=""),
                    header=T,sep=",")

# CRU grids added
fiaCRU <- exp_to_cru(fiaDF)

# aggregate exponents onto CRU grids
cruPCA <- read.table(paste(destDir, "/CRU_with_PCA.csv", sep=""),
                     header=T,sep=",")

fia_agg <- aggreCRU(fiaCRU, cruPCA)

# plot climate impacts
pdf(paste(getwd(), "/FIA_statistical_tests.pdf", sep=""),
    width = 8, height = 8)
fia_plot(fia_agg)
dev.off()

####################################################################################
# create pdf file of Australia maps
pdf(paste(getwd(), "/Oz_maps_of_climate.pdf", sep=""),
    width = 10, height = 8)
ozDF <- Aus_Plot(plotDF)
dev.off()
####################################################################################
## Ozflux site extractions
ozDF <- ozflux_extraction(plotDF)

# create pdf file of Australia maps
pdf(paste(getwd(), "/Oz_barplot.pdf", sep=""))
OzPlot(ozDF)
dev.off()

####################################################################################
# Extract Emerald and Gingin time series raw climate and plot
oz_summary <- TwoSites_summary(ozDF)

# create pdf file of Australia maps
pdf(paste(getwd(), "/Oz_timeseries.pdf", sep=""),
    width = 10, height = 8)
TwoSites_timeseries(ozDF)
dev.off()

####################################################################################
# Extract Tasmania and check with the global climate
pdf(paste(getwd(), "/Tasmania_maps.pdf", sep=""),
    width = 10, height = 8)
tasDF <- Tasmania_process(plotDF)
dev.off()

pdf(paste(getwd(), "/Tasmania_compare.pdf", sep=""),
    width = 10, height = 8)
Tasmania_compare(plotDF)
dev.off()

####################################################################################
# Extract Australia and check with the global climate
pdf(paste(getwd(), "/Australia_maps.pdf", sep=""),
    width = 14, height = 12)
AusDF <- Australia_process(plotDF)
dev.off()


pdf(paste(getwd(), "/Australia_compare.pdf", sep=""),
    width = 10, height = 8)
Australia_compare(plotDF)
dev.off()


# Extract Australia and check with the global climate
pdf(paste(getwd(), "/image/Australia_maps.pdf", sep=""),
    width = 14, height = 10)
AusDF <- Australia_process_map(plotDF)
dev.off()

pdf(paste(getwd(), "/image/Australia_4.pdf", sep=""))
Australia_compare_4(plotDF)
dev.off()

pdf(paste(getwd(), "/image/Australia_7.pdf", sep=""))
Australia_compare_7(plotDF)
dev.off()

pdf(paste(getwd(), "/image/Australia_8.pdf", sep=""))
Australia_compare_8(plotDF)
dev.off()


pdf(paste(getwd(), "/image/Australia_12.pdf", sep=""))
Australia_compare_12(plotDF)
dev.off()


pdf(paste(getwd(), "/image/Australia_13.pdf", sep=""))
Australia_compare_13(plotDF)
dev.off()

####################################################################################
# Checking if P controls biome differences when temp and prec failed
pdf(paste(getwd(), "/BiomeDiffer.pdf", sep=""))
BiomeDifferPlot(plotDF)
dev.off()

####################################################################################
# Compare 2 biomes at one time where they overlap in MAT and MAP
# if prec P and temp P differ among bomes
# and the subsequent C and M relationships
# Plot a matrix and fill color
pdf(paste(getwd(), "/BiomeDiffer_Stats.pdf", sep=""))
BiomeDifferStats(plotDF)
dev.off()

####################################################################################
# Plot 3d checking MAT/MAP vs C vs M
pdf(paste(getwd(), "/plot_3d_MAT.pdf", sep=""))
plot3d_MAT(plotDF)
dev.off()

pdf(paste(getwd(), "/plot_3d_MAP.pdf", sep=""))
plot3d_MAP(plotDF)
dev.off()

####################################################################################
# Calculate coefficient of variation for all data,
# interannual variation, intraannual variation
# Temperature
tempVar <- CoefVar(inDF = read.table(paste(getwd(), "/temp_DF.csv", sep=""),
                                     sep=",", header=T))

# Precipitation
precVar <- CoefVar(inDF = read.table(paste(getwd(), "/pre_DF.csv", sep=""),
                                     sep=",", header=T))


newDF <- tempVar

#newDF <- newDF[order(newDF$mon_coef),]
newDF[newDF$mon_coef >= 10, "mon_coef"] <- 10
newDF[newDF$mon_coef <= -10, "mon_coef"] <- -10

newDF$mon_coef_log <- rescale(newDF$mon_coef, to=c(-1,1))
with(newDF, quilt.plot(lon, lat, mon_coef))

newDF[newDF$inter_coef >= 2, "inter_coef"] <- 2
newDF[newDF$inter_coef <= -2, "inter_coef"] <- -2
with(newDF, quilt.plot(lon, lat, inter_coef))


newDF[newDF$intra_coef >= 1, "intra_coef"] <- 1
newDF[newDF$intra_coef <= -1, "intra_coef"] <- -1
with(newDF, quilt.plot(lon, lat, intra_coef))

####################################################################################



####################################################################################
## Animated plots

#  setwd
setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/density")
# Plot animated density plot
AnimatedDensityPlot(plotDF)
## convert into animation
system("convert -delay 100 *.png animated.gif")

#  setwd
setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/radar")
# plot animated radar plot
Animated_radar(summary)
## convert into animation
system("convert -delay 100 *.png animated.gif")


#  setwd
setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/PCA")
## convert into animation
system("convert -delay 100 *.png animated.gif")

#  setwd
setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/IE")
## convert into animation
system("convert -delay 100 *.png animated.gif")

####################################################################################
# Conver image pdf into jpeg files
FileDir <- paste(getwd(), "/image/", sep="")

pdfTOpng(FileDir)

####################################################################################
# turn warning message back on
options(warn=0)

#### Clear workspace
rm(list=ls(all=TRUE))
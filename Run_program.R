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


####################################################################################
#### Compute Colwell index for temperature and precipitation data
# temperature
PCM_temp(sourceDir = dataDir, destDir = dataDir)

# precipitation
PCM_prec(sourceDir = dataDir, destDir = dataDir)

####################################################################################
#### Calculate climate annual mean and annual sums
# temperature
tempMeans(inFile=paste0(dataDir, "/temp_DF.csv"),
          outFile=paste0(dataDir, "/temp_DF_annual_mean.csv"))

# precipitation
precMeanSums(inFile=paste0(dataDir, "/prec_DF.csv"),
             outFile=paste0(dataDir, "/prec_DF_annual_sum.csv"))

####################################################################################
#### Combine all dataframes
### Step 1. Project BIOME information onto PCM file
biomeProject(corFile=paste0(corDir, "/CRU_Biome.csv"),    # where does this come from?
             tempFile=paste0(dataDir, "/temp_PCM.csv"),
             precFile=paste0(dataDir, "/pre_PCM.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM.csv"))

### Step 2. Save PCM with prec means and sums and temp means
match_climate(tempFile=paste0(dataDir, "/temp_DF_annual_mean.csv"),
             precFile=paste0(dataDir, "/pre_DF_annual_sum.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM.csv"),
             fullFile=paste0(dataDir, "/biome_temp_prec_full.csv"))

### Step 3. Preliminary processing of the df for plotting and table purposes
# generate temp and prec classes and ie factor
plotDF <- classPrep(inPath=paste0(dataDir, "/biome_temp_prec_full.csv"))

####################################################################################
#### Compute summary statistics and figures
### Step 1. Calculate summary df
summary <- summaryPrep(plotDF)

### Step 2. Compute the min and max for summary statistics 
#minDF <- summaryPrep_min(plotDF)
#maxDF <- summaryPrep_max(plotDF)

### Step 3. Basic summary figures
# plot 2d with two directional error bars
pdf(paste0(analysesDir, "/summary_2_d.pdf"),
    width = 10, height = 8)
summary2dimage(summary)
dev.off()

# Plot radar plots for summary data, 1 biome per plot
pdf(paste0(analysesDir, "/radar_plot_individual.pdf"))
radar_summary(summary)
dev.off()


# Plot radar plots for summary data, all biomes 1 plot
pdf(paste0(analysesDir, "/radar_plot_combined.pdf"),
    width = 8, height = 12)
radar_summary_image2(summary)
dev.off()

# Plot star plots for summary data
pdf(paste0(analysesDir, "/star_plot.pdf"))
star_summary_image(summary)
dev.off()

# PCA analysis for each biome using summary statistics
pdf(paste0(analysesDir, "/PCA_summary.pdf"))
SummaryPCA(summary)
dev.off()

pdf(paste0(analysesDir, "/PCA_summary_manuscript_figure.pdf"))
SummaryPCA_image(summary)
dev.off()


####################################################################################
#### Plot all the globally gridded maps, steps 3-4 just split step 2
### Step 1. Plot global biome distribution
pdf(paste0(analysesDir, "/global_biome_plot.pdf"),
    width=10, height=8)
biome_plot(plotDF)
dev.off()

# Step 2. Create pdf file of all climate-related global maps
pdf(paste0(analysesDir, "/global_maps_of_all_climate.pdf"),
    width = 10, height = 8)
PlotMaps(plotDF)
dev.off()


# Step 3. Create pdf file of global maps
pdf(paste0(analysesDir, "/global_maps_of_PCM.pdf"),
    width = 10, height = 9)
plot_PCM_maps(plotDF)
dev.off()

# Step 4. create pdf file of global maps
pdf(paste0(analysesDir, "/global_maps_of_annual_climate.pdf"),
    width = 8, height = 6)
plot_maps_annual(plotDF)
dev.off()


####################################################################################
#### Conduct biome-specific analyses and plottings

### Step 1.  Plot all data points on 2-d space
# 2-d basic plot
pdf(paste0(analysesDir, "/Biome_all_normal.pdf"))
plot_biome_all(plotDF)
dev.off()

# 2-d bagplot
pdf(paste0(analysesDir, "/Biome_bagplot_normal.pdf"),
    width = 22, height = 26)
biome_bag_plot(plotDF)
dev.off()

# 2-d density plot  - takes very long to run!
pdf(paste0(analysesDir, "/Biome_density_normal.pdf"),
    width = 22, height = 26)
biome_density_plot(plotDF)
dev.off()


### Step 2. kernel density plot
pdf(paste0(analysesDir, "/kernel_density.pdf"))
kernel_multi(plotDF)
dev.off()

### Step 3. biome specific Ie factor plot
pdf(paste0(analysesDir, "/biome_Ie.pdf"))
Ieplot(plotDF)
dev.off()

### Step 4. PCA analysis  - takes very long to run!
# PCA analysis for all data, and output PC12 onto CRU grids
pdf(paste0(analysesDir, "/PCA_all_analysis.pdf"))
TotalPCA(plotDF)
dev.off()


# biome specific PCA analysis - takes very long to run!
pdf(paste0(analysesDir, "/PCA_analysis.pdf"))
BiomePCA(plotDF)
dev.off()


####################################################################################
#### Prepare subset of data: only the 50th percentile closest to mean of:
#### 1. temp vs. prec
#### 2. temp P vs. prec P
#### 3. temp C vs. prec C
#### 4. temp M vs. prec M

### Step 1. subsetting
abs_sub <- Percentile50_absolute(plotDF)
p_sub <- Percentile50_p(plotDF)
c_sub <- Percentile50_c(plotDF)
m_sub <- Percentile50_m(plotDF)

### Step 2. plotting
# Plot bagplot 50th percentile data
pdf(paste0(analysesDir, "/Biome_bagplot_50.pdf"),
    width = 22, height = 26)
plot50th(abs_sub, p_sub, plotDF)
dev.off()

# Plot spatially the 50th percentile data
pdf(paste0(analysesDir, "/Spatial_50th.pdf"),
    width = 28, height = 16)
spatial50(abs_sub, p_sub)
dev.off()


####################################################################################
#### Case studies to illustrate usefulness of colwell index 
### Case 1. Australian ozflux sites
# Step 1. Australia continental climate maps
pdf(paste0(analysesDir, "/Oz_maps_of_climate.pdf"),
    width = 10, height = 8)
Aus_Plot(plotDF)
dev.off()

# Step 2. Ozflux site extractions
ozDF <- ozflux_extraction(plotDF)

# Step 3. plot ozflux comparisons
pdf(paste0(analysesDir, "/Oz_barplot.pdf"))
OzPlot(ozDF)
dev.off()

# Step 4. two site comparison - extracting points  - may not needed
oz_summary <- TwoSites_summary(ozDF)

# step 5. two site time series plot
pdf(paste0(analysesDir, "/Oz_timeseries.pdf"),
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
#### End of analysis, restoring settings

### Step 1. turn warning message back on
options(warn=0)

### Step 2. restore par information
par(opar)

#### Step 3. Clear workspace
rm(list=ls(all=TRUE))

####################################################################################
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
#### Downloading global gridded CRU TS3.21 temperature and precipitation data
#### Assume you have downloaded the data as registration is required to download the data
#### Put the raw .nc data into folder "/data/raw_data"
download_CRU_data()

####################################################################################
#### Preliminary processing CRU climate data 

### Step 1. convert from nc to csv file format
## CRU temperature
nc_to_csv(inFile=paste0(ncDir, "/cru_ts3.21.1901.2012.tmp.dat.nc"),
          outFile=paste0(dataDir,"/temp_DF.csv"))

# CRU precipitation
nc_to_csv(inFile=paste0(ncDir, "/cru_ts3.21.1901.2012.pre.dat.nc"),
          outFile=paste0(dataDir,"/prec_DF.csv"))

####################################################################################
#### Compute Colwell index for temperature and precipitation data
### Entire period of 1901-2012
# temperature - Run time: ~ 3 hour 
PCM_temp(sourceDir = dataDir, destDir = dataDir)

# precipitation - Run time: ~ 3 hour 
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
biomeProject(corFile=paste0(corDir, "/CRU_Biome.csv"),    
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

### Step 2. Basic summary figures
# plot 2d with two directional error bars
pdf(paste0(analysesDir, "/summary_2_d.pdf"),
    width = 12, height = 8)
summary_2d_image(summary)
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
# 2-d bagplot
pdf(paste0(analysesDir, "/Biome_bagplot_normal.pdf"),
    width = 22, height = 26)
biome_bag_plot(plotDF)
dev.off()

# 2-d density plot  
# A very time-consuming processing if use the entire dataframe
# hence, use a subset of data by drawing random points from the data
# currently the number of points to be randomly chosen is n = 1000
pdf(paste0(analysesDir, "/Biome_density_normal.pdf"),
    width = 22, height = 26)
biome_density_plot(plotDF)
dev.off()


### Step 2. kernel density plot
pdf(paste0(analysesDir, "/kernel_density.pdf"))
kernel_multi(plotDF)
dev.off()


### Step 3. PCA analysis 
# PCA analysis for all data, and output PC12 onto CRU grids
# A very time-consuming processing if use the entire dataframe
# hence, use a subset of data by drawing random points from the data
# number of points drawn are shown in plots
pdf(paste0(analysesDir, "/PCA_all_analysis.pdf"))
TotalPCA(plotDF)
dev.off()


# biome specific PCA analysis 
# A very time-consuming processing if use the entire dataframe
# hence, use a subset of data by drawing random points from the data
# currently the number of points to be randomly chosen is n = 1000
pdf(paste0(analysesDir, "/PCA_analysis.pdf"))
BiomePCA(plotDF)
dev.off()


####################################################################################
#### Case studies to illustrate usefulness of colwell index 

### Case 3. Australia
# Step 1. 
# Extract Australia and check with the global climate
pdf(paste0(analysesDir, "/Australia_maps_image.pdf"),
    width = 14, height = 10)
Australia_process_map(plotDF)
dev.off()

# Step 2. Biome #4
pdf(paste0(analysesDir, "/Australia_4.pdf"))
Australia_compare_4(plotDF)
dev.off()

# Step 3. Biome #7
pdf(paste0(analysesDir, "/Australia_7.pdf"))
Australia_compare_7(plotDF)
dev.off()

# Step 4. Biome #8
pdf(paste0(analysesDir, "/Australia_8.pdf"))
Australia_compare_8(plotDF)
dev.off()

# Step 5. Biome #12
pdf(paste0(analysesDir, "/Australia_12.pdf"))
Australia_compare_12(plotDF)
dev.off()

# Step 6. Biome #13
pdf(paste0(analysesDir, "/Australia_13.pdf"))
Australia_compare_13(plotDF)
dev.off()

####################################################################################
#### Checking if P controls biome differences when temp and prec failed

# Step 1. Compare 2 biomes at one time where they overlap in MAT and MAP
#         if prec P and temp P differ among bomes
#         and the subsequent C and M relationships
#         Plot a matrix and fill color
pdf(paste0(analysesDir, "/BiomeDiffer_Stats.pdf"))
BiomeDifferStats(plotDF)
dev.off()

####################################################################################
#### Sensitivity analysis of predictability over time
### Step 1. Sourcing two period analyses codes
### Default setting: do not perform two-periods sensitivity analysis
two_periods_analysis <- F
if(two_periods_analysis == T) {
    source("R/Two_period_comparison_script.R")
}


####################################################################################
#### End of analysis, restoring settings
### Step 1. turn warning message back on
options(warn=0)

### Step 2. restore par information
par(opar)

#### Step 3. Clear workspace
rm(list=ls(all=TRUE))

####################################################################################
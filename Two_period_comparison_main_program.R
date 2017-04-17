#### Supplementary program to compare two periods
#### Purpose: 
#### 1. to compute Colwell index for two periods (1901-1990 and 1991-2012)
#### 2. to compare how climatic spaces differ over the two periods
####
#### Author: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 

####################################################################################
#### Make sure everything is clear
rm(list=ls(all=TRUE))

#### prepare wk spaces and libraries
source("R/prepare_R.R")

#### Assume the main program has completed

####################################################################################
#### Compute Colwell index for temperature and precipitation data
### Step 2. period of 1991-2012
# temperature - Run time: ~ 1 hour 
PCM_temp_1991_2012(sourceDir = dataDir, destDir = dataDir)

# precipitation - Run time: ~ 1 hour 
PCM_prec_1991_2012(sourceDir = dataDir, destDir = dataDir)


### step 3. period of 1901-1990
# temperature - Run time: ~ 2.5 hour 
PCM_temp_1901_1990(sourceDir = dataDir, destDir = dataDir)

# precipitation - Run time: ~ 2.5 hour 
PCM_prec_1901_1990(sourceDir = dataDir, destDir = dataDir)

### Step 4. compare the spatial and biome-specific differences between the two time period
# spatial comparisons
pdf(paste0(analysesDir, "/two_period_spatial_comparisons.pdf"),
    width = 10, height = 8)
two_period_spatial_diff()
dev.off()

# biome-specific comparisons
pdf(paste0(analysesDir, "/two_period_biome_comparisons.pdf"),
    width = 10, height = 8)
two_period_biome_diff()
dev.off()

####################################################################################
#### Combine all dataframes
### Step 1. Project BIOME information onto PCM file
# 1901-1990
biomeProject(corFile=paste0(corDir, "/CRU_Biome.csv"),    # where does this come from?
             tempFile=paste0(dataDir, "/temp_PCM_1901_1990.csv"),
             precFile=paste0(dataDir, "/pre_PCM_1901_1990.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM_1901_1990.csv"))

# 1991-2012
biomeProject(corFile=paste0(corDir, "/CRU_Biome.csv"),    # where does this come from?
             tempFile=paste0(dataDir, "/temp_PCM_1991_2012.csv"),
             precFile=paste0(dataDir, "/pre_PCM_1991_2012.csv"), 
             pcmFile=paste0(dataDir, "/biome_temp_prec_PCM_1991_2012.csv"))

### Step 2. Save PCM with prec means and sums and temp means
# 1901-1990
match_climate(tempFile=paste0(dataDir, "/temp_DF_annual_mean.csv"),
              precFile=paste0(dataDir, "/pre_DF_annual_sum.csv"), 
              pcmFile=paste0(dataDir, "/biome_temp_prec_PCM_1901_1990.csv"),
              fullFile=paste0(dataDir, "/biome_temp_prec_full_1901_1990.csv"))
# 1991-2012
match_climate(tempFile=paste0(dataDir, "/temp_DF_annual_mean.csv"),
              precFile=paste0(dataDir, "/pre_DF_annual_sum.csv"), 
              pcmFile=paste0(dataDir, "/biome_temp_prec_PCM_1991_2012.csv"),
              fullFile=paste0(dataDir, "/biome_temp_prec_full_1991_2012.csv"))

### Step 3. Preliminary processing of the df for plotting and table purposes
# generate temp and prec classes and ie factor
plotDF1 <- classPrep(inPath=paste0(dataDir, "/biome_temp_prec_full_1901_1990.csv"))
plotDF2 <- classPrep(inPath=paste0(dataDir, "/biome_temp_prec_full_1991_2012.csv"))

####################################################################################
#### Compute summary statistics and figures
### Step 1. Calculate summary df
summary1 <- summaryPrep(plotDF1)
summary2 <- summaryPrep(plotDF2)

### Step 2. Basic summary figures
# plot 2d with two directional error bars
pdf(paste0(analysesDir, "/summary_2_d_1901_1990.pdf"),
    width = 10, height = 8)
summary_2d_image(summary1)
dev.off()

pdf(paste0(analysesDir, "/summary_2_d_1991_2012.pdf"),
    width = 10, height = 8)
summary_2d_image(summary2)
dev.off()

# Plot radar plots for summary data, all biomes 1 plot
pdf(paste0(analysesDir, "/radar_plot_combined_1901_1990.pdf"),
    width = 8, height = 12)
radar_summary_image2(summary1)
dev.off()

pdf(paste0(analysesDir, "/radar_plot_combined_1991_2012.pdf"),
    width = 8, height = 12)
radar_summary_image2(summary2)
dev.off()

# PCA analysis for each biome using summary statistics
pdf(paste0(analysesDir, "/PCA_summary_1901_1990.pdf"))
SummaryPCA(summary1)
dev.off()

pdf(paste0(analysesDir, "/PCA_summary_1991_2012.pdf"))
SummaryPCA(summary2)
dev.off()

# PCA for manuscript 
pdf(paste0(analysesDir, "/PCA_summary_manuscript_figure_1901_1990.pdf"))
SummaryPCA_image(summary1)
dev.off()

pdf(paste0(analysesDir, "/PCA_summary_manuscript_figure_1991_2012.pdf"))
SummaryPCA_image(summary2)
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

### Case 2. Tasmania
# Step 1. Extract Tasmania and check with the global climate
pdf(paste0(analysesDir, "/Tasmania_maps.pdf"),
    width = 10, height = 8)
Tasmania_process(plotDF)
dev.off()

# Step 2. compare Tasmania to other regions
pdf(paste0(analysesDir, "/Tasmania_compare.pdf"),
    width = 10, height = 8)
Tasmania_compare(plotDF)
dev.off()

### Case 3. Australia
# Step 1. Extract Australia and check with the global climate
pdf(paste0(analysesDir, "/Australia_maps.pdf"),
    width = 14, height = 12)
Australia_process(plotDF)
dev.off()

# Step 2. 
pdf(paste0(analysesDir, "/Australia_compare.pdf"),
    width = 10, height = 8)
Australia_compare(plotDF)
dev.off()

# Step 3. 
# Extract Australia and check with the global climate
pdf(paste0(analysesDir, "/Australia_maps_image.pdf"),
    width = 14, height = 10)
Australia_process_map(plotDF)
dev.off()

# Step 4. Biome #4
pdf(paste0(analysesDir, "/Australia_4.pdf"))
Australia_compare_4(plotDF)
dev.off()

# Step 5. Biome #7
pdf(paste0(analysesDir, "/Australia_7.pdf"))
Australia_compare_7(plotDF)
dev.off()

# Step 6. Biome #8
pdf(paste0(analysesDir, "/Australia_8.pdf"))
Australia_compare_8(plotDF)
dev.off()

# Step 7. Biome #12
pdf(paste0(analysesDir, "/Australia_12.pdf"))
Australia_compare_12(plotDF)
dev.off()

# Step 8. Biome #13
pdf(paste0(analysesDir, "/Australia_13.pdf"))
Australia_compare_13(plotDF)
dev.off()

####################################################################################
#### Checking if P controls biome differences when temp and prec failed
# Step 1. Individual biome comparison - not recommended to run
pdf(paste0(analysesDir, "/BiomeDiffer.pdf"))
BiomeDifferPlot(plotDF)
dev.off()

# Step 2. Compare 2 biomes at one time where they overlap in MAT and MAP
#         if prec P and temp P differ among bomes
#         and the subsequent C and M relationships
#         Plot a matrix and fill color
pdf(paste0(analysesDir, "/BiomeDiffer_Stats.pdf"))
BiomeDifferStats(plotDF)
dev.off()

####################################################################################
#### End of analysis, restoring settings
### Step 1. turn warning message back on
options(warn=0)

### Step 2. restore par information
par(opar)

#### Step 3. Clear workspace
rm(list=ls(all=TRUE))

####################################################################################
#### Supplementary program to compare two periods
#### Purpose: 
#### 1. to compute Colwell index for two periods (1901-1990 and 1991-2012)
#### 2. to compare how climatic spaces differ over the two periods
####
#### Author: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 

####################################################################################

if(start_from_raw_data == T) {
    #### Compute Colwell index for temperature and precipitation data
    ### Step 1. period of 1991-2012
    # temperature - Run time: ~ 1 hour 
    PCM_temp_1991_2012(sourceDir = dataDir, destDir = dataDir)
    
    # precipitation - Run time: ~ 1 hour 
    PCM_prec_1991_2012(sourceDir = dataDir, destDir = dataDir)
    
    
    ### step 2. period of 1901-1990
    # temperature - Run time: ~ 2.5 hour 
    PCM_temp_1901_1990(sourceDir = dataDir, destDir = dataDir)
    
    # precipitation - Run time: ~ 2.5 hour 
    PCM_prec_1901_1990(sourceDir = dataDir, destDir = dataDir)
    
    ### Step 3. compare the spatial and biome-specific differences between the two time period
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
}


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
pdf(paste0(plotDir, "/summary_2_d_two_period_comparison.pdf"),
    width = 10, height = 8)
summary_2d_image_comparison(summary1, summary2)
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

# PCA for manuscript 
pdf(paste0(analysesDir, "/PCA_summary_two_period_comparison.pdf"))
SummaryPCA_image_comparison(summary1, summary2)
dev.off()


####################################################################################
#### Checking if P controls biome differences when temp and prec failed
pdf(paste0(analysesDir, "/BiomeDiffer_Stats_two_period_comparison.pdf"))
BiomeDifferStats_comparison(plotDF1, plotDF2)
dev.off()


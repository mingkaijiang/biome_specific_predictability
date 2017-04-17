####################################################################################
## Prepare classes for plotting temp and prec data
classPrep <- function(inPath) {
    
    # read in full dataset (gridded temperature means, precipitation means and sums, temp and prec PCM)
    inDF <- read.table(inPath, 
                       header=T,sep=",")
    
    colnames(inDF) <- c("CRU_Site", "lon", "lat", "tempP",
                        "tempC", "tempM", "BIOME", "precP",
                        "precC", "precM", "temp", "prec_sum", "prec_mean")
    
    # prepare breaking points for temperature and precipitation
    temp.lab <- c("<-4.7", "-2.1", "0.5", "3.0", "5.6", "8.2",
                  "10.8", "13.4", "16.0", "18.5", "21.1", ">21.1")
    prec.lab <- c("0", "2.3", "5.3", "12.2", "28", "64",
                  "148", "340", "783", "1801", "4142",
                  ">4142")
    
    # prepare temp and prec label in the dataframe
    plotDF <- inDF
    
    plotDF$templab <- ifelse(plotDF$temp < -4.725, 1, 
                             ifelse(plotDF$temp >= -4.725 & plotDF$temp < -2.14, 2,
                                    ifelse(plotDF$temp >= -2.14 & plotDF$temp < 0.445, 3, 
                                           ifelse(plotDF$temp >= 0.445 & plotDF$temp < 3.03, 4, 
                                                  ifelse(plotDF$temp >= 3.03 & plotDF$temp < 5.615, 5, 
                                                         ifelse(plotDF$temp >= 5.615 & plotDF$temp < 8.2, 6, 
                                                                ifelse(plotDF$temp >= 8.2 & plotDF$temp < 10.785, 7,
                                                                       ifelse(plotDF$temp >= 10.785 & plotDF$temp < 13.37, 8, 
                                                                              ifelse(plotDF$temp >= 13.37 & plotDF$temp < 15.955, 9, 
                                                                                     ifelse(plotDF$temp >= 15.955 & plotDF$temp < 18.34, 10,
                                                                                            ifelse(plotDF$temp >= 18.34 & plotDF$temp < 21.125, 11,
                                                                                                   12)))))))))))
    
    plotDF$preclab <- ifelse(plotDF$prec_sum == 0 , 1, 
                             ifelse(plotDF$prec_sum >= 0 & plotDF$prec_sum < 2.3, 2,
                                    ifelse(plotDF$prec_sum >= 2.3 & plotDF$prec_sum < 5.3, 3, 
                                           ifelse(plotDF$prec_sum >= 5.3 & plotDF$prec_sum < 12.2, 4, 
                                                  ifelse(plotDF$prec_sum >= 12.2 & plotDF$prec_sum < 28, 5, 
                                                         ifelse(plotDF$prec_sum >= 28 & plotDF$prec_sum < 64, 6, 
                                                                ifelse(plotDF$prec_sum >= 64 & plotDF$prec_sum < 148, 7,
                                                                       ifelse(plotDF$prec_sum >= 148 & plotDF$prec_sum < 340, 8, 
                                                                              ifelse(plotDF$prec_sum >= 340 & plotDF$prec_sum < 783, 9, 
                                                                                     ifelse(plotDF$prec_sum >= 783 & plotDF$prec_sum < 1801, 10,
                                                                                            ifelse(plotDF$prec_sum >= 1801 & plotDF$prec_sum < 4142, 11,
                                                                                                   12)))))))))))
    
    inDF <- plotDF
    
    #IE for temperature
    inDF$tempMC <- inDF$tempM/inDF$tempC
    inDF$tempIE <- ifelse(inDF$tempMC >=1, 2, 1)
    
    #IE for precipitation
    inDF$precMC <- inDF$precM/inDF$precC
    inDF$precIE <- ifelse(inDF$precMC >=1, 2, 1)
    
    return(inDF)
}
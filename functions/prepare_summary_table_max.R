
####################################################################################
# Process summary dataframe
summaryPrep_max <- function(inDF) {
    ## Prepare biome specific summary dataframe
    summary <- matrix(nrow = 14, ncol = 9)
    summary <- as.data.frame(summary)
    colnames(summary) <- c("biome","tempP_max","tempC_max","tempM_max",
                           "precP_max","precC_max","precM_max",
                           "temp_max","prec_max")
    summary$biome <- c(1:14)
    
    ## read in inDF
    myDF <- inDF
    
    for(i in 1:14) {
        summary[summary$biome == i, "tempP_max"] <- round(max(myDF[myDF$BIOME == i, "tempP"]),2)
        summary[summary$biome == i, "tempC_max"] <- round(max(myDF[myDF$BIOME == i, "tempC"]),2)
        summary[summary$biome == i, "tempM_max"] <- round(max(myDF[myDF$BIOME == i, "tempM"]),2)
        summary[summary$biome == i, "precP_max"] <- round(max(myDF[myDF$BIOME == i, "precP"]),2)
        summary[summary$biome == i, "precC_max"] <- round(max(myDF[myDF$BIOME == i, "precC"]),2)
        summary[summary$biome == i, "precM_max"] <- round(max(myDF[myDF$BIOME == i, "precM"]),2)
        summary[summary$biome == i, "temp_max"] <- round(max(myDF[myDF$BIOME == i, "temp"]),2)
        summary[summary$biome == i, "prec_max"] <- round(max(myDF[myDF$BIOME == i, "prec_sum"]),2)
    }
    
    return(summary)
}
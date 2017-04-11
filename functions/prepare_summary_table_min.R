####################################################################################
# Process summary dataframe
summaryPrep_min <- function(inDF) {
    ## Prepare biome specific summary dataframe
    summary <- matrix(nrow = 14, ncol = 9)
    summary <- as.data.frame(summary)
    colnames(summary) <- c("biome","tempP_min","tempC_min","tempM_min",
                           "precP_min","precC_min","precM_min",
                           "temp_min","prec_min")
    summary$biome <- c(1:14)
    
    ## read in inDF
    myDF <- inDF
    
    for(i in 1:14) {
        summary[summary$biome == i, "tempP_min"] <- round(min(myDF[myDF$BIOME == i, "tempP"]),2)
        summary[summary$biome == i, "tempC_min"] <- round(min(myDF[myDF$BIOME == i, "tempC"]),2)
        summary[summary$biome == i, "tempM_min"] <- round(min(myDF[myDF$BIOME == i, "tempM"]),2)
        summary[summary$biome == i, "precP_min"] <- round(min(myDF[myDF$BIOME == i, "precP"]),2)
        summary[summary$biome == i, "precC_min"] <- round(min(myDF[myDF$BIOME == i, "precC"]),2)
        summary[summary$biome == i, "precM_min"] <- round(min(myDF[myDF$BIOME == i, "precM"]),2)
        summary[summary$biome == i, "temp_min"] <- round(min(myDF[myDF$BIOME == i, "temp"]),2)
        summary[summary$biome == i, "prec_min"] <- round(min(myDF[myDF$BIOME == i, "prec_sum"]),2)
    }
    
    return(summary)
}

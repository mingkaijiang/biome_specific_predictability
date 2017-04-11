####################################################################################
# Process summary dataframe
summaryPrep <- function(inDF) {
    ## Prepare biome specific summary dataframe
    summary <- matrix(nrow = 14, ncol = 17)
    summary <- as.data.frame(summary)
    colnames(summary) <- c("biome","tempP_mean","tempC_mean","tempM_mean",
                           "tempP_sd","tempC_sd","tempM_sd",
                           "precP_mean","precC_mean","precM_mean",
                           "precP_sd","precC_sd","precM_sd",
                           "temp_mean","temp_sd","prec_mean",
                           "prec_sd")
    summary$biome <- c(1:14)
    
    ## read in inDF
    myDF <- inDF
    
    for(i in 1:14) {
        summary[summary$biome == i, "tempP_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempP"]),2)
        summary[summary$biome == i, "tempC_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempC"]),2)
        summary[summary$biome == i, "tempM_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempM"]),2)
        summary[summary$biome == i, "precP_mean"] <- round(mean(myDF[myDF$BIOME == i, "precP"]),2)
        summary[summary$biome == i, "precC_mean"] <- round(mean(myDF[myDF$BIOME == i, "precC"]),2)
        summary[summary$biome == i, "precM_mean"] <- round(mean(myDF[myDF$BIOME == i, "precM"]),2)
        summary[summary$biome == i, "tempP_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempP"]),2)
        summary[summary$biome == i, "tempC_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempC"]),2)
        summary[summary$biome == i, "tempM_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempM"]),2)
        summary[summary$biome == i, "precP_sd"] <- round(sd(myDF[myDF$BIOME == i, "precP"]),2)
        summary[summary$biome == i, "precC_sd"] <- round(sd(myDF[myDF$BIOME == i, "precC"]),2)
        summary[summary$biome == i, "precM_sd"] <- round(sd(myDF[myDF$BIOME == i, "precM"]),2)
        summary[summary$biome == i, "temp_mean"] <- round(mean(myDF[myDF$BIOME == i, "temp"]),2)
        summary[summary$biome == i, "prec_mean"] <- round(mean(myDF[myDF$BIOME == i, "prec_sum"]),2)
        summary[summary$biome == i, "temp_sd"] <- round(sd(myDF[myDF$BIOME == i, "temp"]),2)
        summary[summary$biome == i, "prec_sd"] <- round(sd(myDF[myDF$BIOME == i, "prec_sum"]),2)
    }
    
    ## save onto disk
    write.table(summary, paste0(dataDir, "/summary_statistics.csv"),
                col.names=T,row.names=F, sep=",")
    
    return(summary)
}

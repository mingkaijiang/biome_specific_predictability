####################################################################################
## Prepare 50th percentile data for temp vs. prec dataframe
Percentile50_absolute <- function(inDF) {
    ## read in file
    myDF <- inDF
    
    ## setting summary dataframe
    summary <- matrix(nrow = 14, ncol = 4)
    summary <- as.data.frame(summary)
    colnames(summary) <- c("biome","temp_mean","prec_mean","dist_50")
    summary$biome <- c(1:14)
    
    ## Filling biome-specific average values
    for (i in 1:14) {
        summary[summary$biome == i, "temp_mean"] <- mean(myDF[myDF$BIOME == i, "temp"])
        summary[summary$biome == i, "prec_mean"] <- mean(myDF[myDF$BIOME == i, "prec_sum"])
    }
    
    ## Calculating distances
    i <- 1
    
    newDF <- subset(myDF, BIOME == i)
    temp <- summary[summary$biome == i, "temp_mean"]
    prec <- summary[summary$biome == i, "prec_mean"]
    newDF$dist <- sqrt((temp - newDF$temp)^2 + 
                           (prec - newDF$prec_sum)^2)
    output <- newDF
    
    for (i in 2:14)
    {
        newDF <- subset(myDF, BIOME == i)
        temp <- summary[summary$biome == i, "temp_mean"]
        prec <- summary[summary$biome == i, "prec_mean"]
        newDF$dist <- sqrt((temp - newDF$temp)^2 + 
                               (prec - newDF$prec_sum)^2)
        
        output <- rbind(output, newDF)
    }
    
    ## calculating the quantiles and the subset
    
    for (i in 1:14)
    {
        summary[summary$biome == i, "dist_50"] <- quantile(output[output$BIOME == i, "dist"], 0.5)
        output[output$BIOME == i, "subset"] <- summary[summary$biome == i, "dist_50"] - output[output$BIOME == i, "dist"]
        
    }
    
    ## subset output with distance smaller than 50th percentile
    subDF <- subset(output, subset >= 0)
    
    return(subDF)
}

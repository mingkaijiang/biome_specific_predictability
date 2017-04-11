####################################################################################
## Calculate annual mean and annual sums for precipitation
precMeanSums <- function(inFile, outFile) {
    precdf <- fread(inFile, 
                         header=T, sep=",")
    
    precdf <- as.data.frame(precdf)
    
    precdf$annual_sum <- round(rowSums(precdf[,5:16]),2)
    precdf$annual_mean <- round(rowMeans(precdf[,5:16]),2)
    
    outdf <- subset(precdf, year == 1901)
    outdf <- outdf[,1:4]
    
    annual_sum<-aggregate(precdf$annual_sum, by = list(precdf$CRU_Site),
                          mean, na.rm = TRUE)
    annual_mean<-aggregate(precdf$annual_mean, by = list(precdf$CRU_Site),
                           mean, na.rm = TRUE)
    
    outdf$annual_sum <- annual_sum[,2]
    outdf$annual_mean <- annual_mean[,2]
    colnames(outdf)<- c("CRU_Site", "lon", "lat", "year", "annual_sum", "annual_mean")
    write.table(outdf, outFile,
                row.names=F, col.names=T, sep=",")
}

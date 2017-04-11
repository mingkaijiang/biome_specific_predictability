####################################################################################
## Calculate annual mean for temperature
tempMeans <- function(inFile, outFile) {
    
    tempdf <- read.table(inFile, 
                         header=T, sep=",")
    tempdf$annual_mean <- round(rowMeans(tempdf[,5:16]),2)
    
    outdf <- subset(tempdf, year == 1901)
    outdf <- outdf[,1:4]
    for (i in outdf$CRU_Site)
    {
        outdf[outdf$CRU_Site == i, "annual_mean"] <- round(mean(tempdf[tempdf$CRU_Site == i, "annual_mean"]),2)
    }
    
    write.table(outdf, outFile,
                row.names=F, col.names=T, sep=",")
}

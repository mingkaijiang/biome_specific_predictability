####################################################################################
## Save PCM with prec mean sums, and temp means
match_climate <- function(tempFile, precFile, pcmFile, fullFile) {
    ## temperature
    tempdf <- read.table(tempFile,
                         header=T,sep=",")
    ## precipitation
    precdf <- read.table(precFile,
                         header=T,sep=",")
    
    ##Process biome specific temp P vs. prec P relationship
    myDF <- read.table(pcmFile, sep=",",
                       header=T)
    
    #myDF$BIOME <- corDF$BIOME
    
    ## Match with CRU_Site reference number
    myDF$temp_annual_mean <- tempdf$annual_mean
    
    myDF$prec_annual_sum <- precdf$annual_sum
    
    myDF$prec_annual_mean <- precdf$annual_mean
    
    
    write.table(myDF, fullFile,
                row.names=F, col.names=T, sep=",")
    
}
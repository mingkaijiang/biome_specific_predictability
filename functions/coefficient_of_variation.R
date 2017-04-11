
####################################################################################
## Calculate coefficient of variation
CoefVar <- function(inDF) {
    # Calculate:
    # monthly coef of var
    # inter-annual coef of var
    # intra-annual coef of var
    
    # create a dtataframe to contain all the data
    outDF <- subset(inDF, year == 1901)
    outDF <- outDF[,1:3]
    
    site.list <- unique(inDF$CRU_Site)
    
    # calculate sd and mean all the above mentioned time period
    for (i in site.list)
    {
        subDF <- subset(inDF, CRU_Site == i)
        
        # monthly sd and mean
        outDF[outDF$CRU_Site == i, "mon_sd"] <- sd(as.matrix(subDF[,5:16]))
        outDF[outDF$CRU_Site == i, "mon_mean"] <- mean(as.matrix(subDF[,5:16]))
        
        # inter-annual sd and mean
        subDF$annual <- rowMeans(subDF[5:16])
        outDF[outDF$CRU_Site == i, "inter_mean"] <- mean(subDF$annual)
        outDF[outDF$CRU_Site == i, "inter_sd"] <- sd(subDF$annual)
        
        # intra-annual sd and mean
        intra <- colMeans(subDF[,5:16])
        outDF[outDF$CRU_Site == i, "intra_mean"] <- mean(intra)
        outDF[outDF$CRU_Site == i, "intra_sd"] <- sd(intra)
    }
    
    # calculate coef of var
    outDF$mon_coef <- outDF$mon_sd/outDF$mon_mean
    outDF$inter_coef <- outDF$inter_sd/outDF$inter_mean
    outDF$intra_coef <- outDF$intra_sd/outDF$intra_mean
    
    return(outDF)
    
}

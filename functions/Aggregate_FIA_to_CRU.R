

####################################################################################
## Aggregate fia exponent data onto CRU grids

aggreCRU <- function(inDF, cru) {
    
    # subset plot r square >= 0.3
    subDF <- subset(inDF, rsq >=0.3)
    subDF <- subset(subDF, num >= 50)
    
    # checking
    #newDF <- subDF[order(subDF$exp),]
    #head(newDF)
    
    ## Grid averages of tree statistical parameters
    expDF <- aggregate(subDF[,"exp", drop=F], subDF[,8:9], mean, na.rm=T)
    intDF <- aggregate(subDF[,"int", drop=F], subDF[,8:9], mean, na.rm=T)
    rsqDF <- aggregate(subDF[,"rsq", drop=F], subDF[,8:9], mean, na.rm=T)
    
    newDF <- data.frame(expDF, intDF$int, rsqDF$rsq)
    colnames(newDF) <- c("LON", "LAT", "exp", "int", "rsq")
    
    for (i in unique(newDF$LON)) {
        for (j in unique(newDF$LAT)) {
            
            grid.check <- length(cru[cru$lon == i & cru$lat == j, "temp"])
            
            newDF[newDF$LON == i & newDF$LAT == j, "CRU_Site"] <- ifelse(grid.check > 0, 
                                                                         cru[cru$lon == i & cru$lat == j, "CRU_Site"],
                                                                         NA)
            newDF[newDF$LON == i & newDF$LAT == j, "tempP"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "tempP"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "tempC"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "tempC"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "tempM"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "tempM"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "BIOME"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "BIOME"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "precP"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "precP"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "precC"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "precC"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "precM"] <- ifelse(grid.check > 0, 
                                                                      cru[cru$lon == i & cru$lat == j, "precM"],
                                                                      NA)
            newDF[newDF$LON == i & newDF$LAT == j, "temp"] <- ifelse(grid.check > 0, 
                                                                     cru[cru$lon == i & cru$lat == j, "temp"],
                                                                     NA)
            newDF[newDF$LON == i & newDF$LAT == j, "prec_sum"] <- ifelse(grid.check > 0, 
                                                                         cru[cru$lon == i & cru$lat == j, "prec_sum"],
                                                                         NA)
            
            newDF[newDF$LON == i & newDF$LAT == j, "PC1"] <- ifelse(grid.check > 0, 
                                                                    cru[cru$lon == i & cru$lat == j, "PC1"],
                                                                    NA)
            
            newDF[newDF$LON == i & newDF$LAT == j, "PC2"] <- ifelse(grid.check > 0, 
                                                                    cru[cru$lon == i & cru$lat == j, "PC2"],
                                                                    NA)
        } # j
    }   # i
    
    return(newDF)
}

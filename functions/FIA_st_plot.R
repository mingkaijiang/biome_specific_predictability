
####################################################################################
## USDA FIA dataset processing
## Not used in main program, just for information purpose
fia_st_plot <- function(cor, fid) {
    ## Function purpose:
    ## Compute plot averages of self-thinning exponents
    ## using all data available
    
    ## library
    #require(raster)
    #require(sm)
    #require(fields)
    #require(spatstat)
    #require(lattice)
    #require(gridExtra)
    
    ## add lon lat onto fid dataframe
    newcor <- data.table(cor$CN, cor$LAT, cor$LON)
    colnames(newcor)<-c("PLT_CN", "LAT", "LON")
    newfid <- merge(fid, newcor, by=c("PLT_CN"))
    
    ## reduce data size by removing NAs and 0s
    subDF <- subset(newfid, STATUSCD == 1)
    subDF <- data.frame(subDF$PLT_CN, subDF$DIA, subDF$TPA_UNADJ)
    colnames(subDF) <- c("PLT_CN", "DIA", "TPA_UNADJ")
    subDF <- subDF[complete.cases(subDF$DIA),]
    subDF <- subDF[complete.cases(subDF$TPA_UNADJ),]
    subDF <- subset(subDF, TPA_UNADJ > 0)
    subDF <- subset(subDF, DIA > 0)
    
    ## checking number of trees within each plot
    freqDF <- as.data.frame(table(subDF$PLT_CN))
    freqDF <- subset(freqDF, Freq >= 30)
    
    ## create cn list
    cn.list <- unique(freqDF$Var1)
    ll <- length(cn.list)
    
    ## create output dataframe
    outDF <- as.data.frame(matrix(ncol=5, nrow=ll))
    colnames(outDF) <- c("PLT_CN", "exp", "int", "rsq", "num")
    outDF$PLT_CN <- cn.list
    
    # exponential curve fitting
    for (i in cn.list) {
        new <- subset(subDF, PLT_CN == i)
        test1 <- lm(log(new$DIA)~log(new$TPA_UNADJ))
        outDF[outDF$PLT_CN == i, "exp"] <- summary(test1)$coefficients[[2]]
        outDF[outDF$PLT_CN == i, "int"] <- summary(test1)$coefficients[[1]]
        outDF[outDF$PLT_CN == i, "rsq"] <- summary(test1)$r.squared
        outDF[outDF$PLT_CN == i, "num"] <- length(new$PLT_CN)
    }
    
    # add coordinates
    final <- merge(outDF, newcor, by=c("PLT_CN"))
    
    return(final)
}

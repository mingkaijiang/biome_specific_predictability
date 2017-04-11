####################################################################################
## USDA FIA dataset processing
## Not used in main program, just for information purpose
fia_st_all <- function(cor, fid) {
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
    
    # taking plot averages
    DF1 <- aggregate(DRYBIO_AG ~ PLT_CN, newfid, mean, na.rm=T)
    DF2 <- aggregate(DRYBIO_BG ~ PLT_CN, newfid, mean, na.rm=T)
    DF3 <- aggregate(DIA ~ PLT_CN, newfid, mean, na.rm=T)
    DF4 <- aggregate(STOCKING ~ PLT_CN, newfid, mean, na.rm=T)
    DF5 <- aggregate(PREVDIA ~ PLT_CN, newfid, mean, na.rm=T)
    DF6 <- aggregate(TPA_UNADJ ~ PLT_CN, newfid, mean, na.rm=T)
    DF7 <- aggregate(CARBON_AG ~ PLT_CN, newfid, mean, na.rm=T)
    DF8 <- aggregate(CARBON_BG ~ PLT_CN, newfid, mean, na.rm=T)
    
    # merging onto original coordinates, based on PLT_CN indexing
    newDF <- merge(DF1, newcor, by=c("PLT_CN"))
    newDF <- merge(newDF, DF2, by=c("PLT_CN"))
    newDF <- merge(newDF, DF3, by=c("PLT_CN"))
    newDF <- merge(newDF, DF4, by=c("PLT_CN"))
    newDF <- merge(newDF, DF5, by=c("PLT_CN"))
    newDF <- merge(newDF, DF6, by=c("PLT_CN"))
    newDF <- merge(newDF, DF7, by=c("PLT_CN"))
    newDF <- merge(newDF, DF8, by=c("PLT_CN"))
    
    # remove NAs
    newDF <- newDF[complete.cases(newDF),]
    
    # exponential function
    test <- lm(log(newDF$DIA)~log(newDF$TPA_UNADJ)) # best model so far
    summary(test)
}  

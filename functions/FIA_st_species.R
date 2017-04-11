
####################################################################################
## USDA FIA dataset processing
fia_st_species <- function(fid) {
    ## Function purpose:
    ## Compute spgrpcd of self-thinning exponents, intercept, and R-square values
    
    # generate species group list
    spgrpcd.list <- unique(fid$SPGRPCD)
    spgrpcd.list <- spgrpcd.list[!is.na(spgrpcd.list)]
    
    # create output dataframe
    ll <- length(spgrpcd.list)
    outDF <- as.data.frame(matrix(ncol=7, nrow=ll))
    colnames(outDF) <- c("spgrpcd", "T1_exp", "T1_int", "T1_rsq",
                         "T2_exp", "T2_int", "T2_rsq")
    outDF$spgrpcd <- spgrpcd.list
    
    # species group level exponential function
    for (i in spgrpcd.list) {
        # subsetting based on species group indexing
        subDF <- subset(fid, SPGRPCD == i)
        
        # keep complete cases only
        subDF <- subDF[complete.cases(subDF$DRYBIO_AG),]
        subDF <- subDF[complete.cases(subDF$DRYBIO_BG),]
        subDF <- subDF[complete.cases(subDF$DIA),]
        subDF <- subDF[complete.cases(subDF$TPA_UNADJ),]
        subDF <- subset(subDF, TPA_UNADJ > 0)
        
        # test 1: drybio_ag + drybio_bg ~ tpa_unadj
        test1 <- lm(log(subDF$DRYBIO_AG + subDF$DRYBIO_BG)~log(subDF$TPA_UNADJ))
        outDF[outDF$spgrpcd == i, "T1_exp"] <- summary(test1)$coefficients[[2]]
        outDF[outDF$spgrpcd == i, "T1_int"] <- summary(test1)$coefficients[[1]]
        outDF[outDF$spgrpcd == i, "T1_rsq"] <- summary(test1)$r.squared
        
        test2 <- lm(log(subDF$DIA)~log(subDF$TPA_UNADJ))
        outDF[outDF$spgrpcd == i, "T2_exp"] <- summary(test2)$coefficients[[2]]
        outDF[outDF$spgrpcd == i, "T2_int"] <- summary(test2)$coefficients[[1]]
        outDF[outDF$spgrpcd == i, "T2_rsq"] <- summary(test2)$r.squared
    }
    
    return(outDF)
}  


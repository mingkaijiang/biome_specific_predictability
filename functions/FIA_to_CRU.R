
####################################################################################
## Project fia exponent dataset onto cru grids
exp_to_cru <- function(inDF) {
    inDF$intlon <- round(inDF[,"LON"], 0)
    inDF$declon <- inDF[,"LON"] - inDF$intlon
    inDF$u <- inDF$intlon + 0.25
    inDF$d <- inDF$intlon + 0.75
    inDF[,"LON_CRU"] <- ifelse(inDF[,"LON"] < inDF[,"u"], inDF$intlon-0.25,
                               ifelse(inDF[, "LON"] >= inDF[,"d"], (inDF$intlon+0.75),
                                      (inDF$intlon+0.25)))
    
    inDF$intlat <- round(inDF[, "LAT"], 0)
    inDF$declat <- inDF[,"LAT"] - inDF$intlat
    inDF$u <- inDF$intlat + 0.25
    inDF$d <- inDF$intlat + 0.75
    inDF[,"LAT_CRU"] <- ifelse(inDF[,"LAT"] < inDF[,"u"], inDF$intlat-0.25,
                               ifelse(inDF[, "LAT"] >= inDF[,"d"], (inDF$intlat+0.75),
                                      (inDF$intlat+0.25)))
    inDF$u <-NULL
    inDF$d <-NULL
    inDF$intlon <- NULL
    inDF$intlat <- NULL
    inDF$declon <- NULL
    inDF$declat <- NULL
    
    return(inDF)
}
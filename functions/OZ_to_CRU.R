###################################################################################
## Save ozflux coordinates onto CRU coordinate system
oz_to_cru <- function(inDF) {
    inDF$intlon <- round(inDF[,"Lon"], 0)
    inDF$declon <- inDF[,"Lon"] - inDF$intlon
    inDF$u <- inDF$intlon + 0.25
    inDF$d <- inDF$intlon + 0.75
    inDF[,"LON_CRU"] <- ifelse(inDF[,"Lon"] < inDF[,"u"], inDF$intlon-0.25,
                               ifelse(inDF[, "Lon"] >= inDF[,"d"], (inDF$intlon+0.75),
                                      (inDF$intlon+0.25)))
    
    inDF$intlat <- round(inDF[, "Lat"], 0)
    inDF$declat <- inDF[,"Lat"] - inDF$intlat
    inDF$u <- inDF$intlat + 0.25
    inDF$d <- inDF$intlat + 0.75
    inDF[,"LAT_CRU"] <- ifelse(inDF[,"Lat"] < inDF[,"u"], inDF$intlat-0.25,
                               ifelse(inDF[, "Lat"] >= inDF[,"d"], (inDF$intlat+0.75),
                                      (inDF$intlat+0.25)))
    inDF$u <-NULL
    inDF$d <-NULL
    inDF$intlon <- NULL
    inDF$intlat <- NULL
    inDF$declon <- NULL
    inDF$declat <- NULL
    
    return(inDF)
}

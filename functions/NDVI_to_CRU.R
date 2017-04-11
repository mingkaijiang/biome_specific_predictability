####################################################################################
## Read in NDVI data and NDVI predictability data

ndvi_to_cru <- function(cruDF, outPath) {
    
    # library
    require(sp)
    require(rgdal)
    
    # in file path
    inPath <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/NDVI"
    
    # NDVI data
    ndvi <- read.table(paste(inPath, "/NDVI_DF.csv", sep=""), 
                       header=T, sep = ",")
    
    # row means
    ndvi$mean <- round(rowMeans(ndvi[,5:16]),2)
    
    # calculate site averages
    newDF <- aggregate(ndvi$mean, by = list(ndvi$Site),
                       mean, na.rm = TRUE)
    
    # NDVI PCM
    ndvip <-read.table(paste(inPath, "/PCM/NDVI_DF.csv", sep=""), 
                       header=T, sep = ",")
    
    ndvip2 <- ndvip[order(ndvip$Site),]
    newDF2 <- newDF[order(newDF$Group.1),]
    
    ndvip <- ndvip2
    newDF <- newDF2
    
    outDF <- data.frame(ndvip$Site, ndvip$lat, ndvip$lon,
                        ndvip$P, ndvip$C, ndvip$M, newDF2$x)
    colnames(outDF) <- c("Site", "lat", "lon", "ndviP",
                         "ndviC", "ndviM", "ndviMean")
    
    # Conver NDVI coordinates to CRU coordinates
    outDF1 <- subset(outDF, lon <= 180)
    outDF2 <- subset(outDF, lon > 180)
    outDF2$lon <- (outDF2$lon - 360)
    newout <- rbind(outDF1, outDF2)
    
    # read in cru dataframe
    corDF <- data.frame(cruDF$lon, cruDF$lat, cruDF$CRU_Site)
    colnames(corDF) <- c("lon", "lat", "CRU_Site")
    
    # convert cru into raster
    coordinates(corDF) <- ~lon+lat
    gridded(corDF) <- T
    r <- raster(corDF)
    
    # convert ndvi P into raster
    subDF <- data.frame(newout$lon, newout$lat, newout$ndviP)
    colnames(subDF) <- c("lon", "lat", "ndviP")
    coordinates(subDF) <- ~lon+lat
    gridded(subDF) <- T
    p <- raster(subDF)
    
    # resampling ndvi P onto cru points
    res <- resample(p, r, method="ngb")
    out <-   as.data.frame(rasterToPoints(res))
    
    # convert ndvi C into raster
    subDF <- data.frame(newout$lon, newout$lat, newout$ndviC)
    colnames(subDF) <- c("lon", "lat", "ndviC")
    coordinates(subDF) <- ~lon+lat
    gridded(subDF) <- T
    p <- raster(subDF)
    
    # resampling ndvi C onto cru points
    res <- resample(p, r, method="ngb")
    out1 <-   as.data.frame(rasterToPoints(res))
    
    # convert ndvi M into raster
    subDF <- data.frame(newout$lon, newout$lat, newout$ndviM)
    colnames(subDF) <- c("lon", "lat", "ndviM")
    coordinates(subDF) <- ~lon+lat
    gridded(subDF) <- T
    p <- raster(subDF)
    
    # resampling ndvi M onto cru points
    res <- resample(p, r, method="ngb")
    out2 <-   as.data.frame(rasterToPoints(res))
    
    # convert ndvi_mean into raster
    subDF <- data.frame(newout$lon, newout$lat, newout$ndviMean)
    colnames(subDF) <- c("lon", "lat", "ndviMean")
    coordinates(subDF) <- ~lon+lat
    gridded(subDF) <- T
    p <- raster(subDF)
    
    # resampling ndvi_mean onto cru points
    res <- resample(p, r, method="ngb")
    out3 <-  as.data.frame(rasterToPoints(res))
    
    # combine ndvi output at CRU grids
    out <- data.frame(out, out1$ndviC, 
                      out2$ndviM, out3$ndviMean)
    colnames(out) <- c("lon", "lat", "ndviP",
                       "ndviC", "ndviM", "ndviMean")
    
    # prepare lon lat list to project out DF onto CRU DF
    lon.list <- unique(out$lon)
    lat.list <- unique(out$lat)
    for (i in lon.list) {
        for (j in lat.list) {
            grid.check <- length(out[out$lon == i & out$lat == j, "ndviP"])
            
            cruDF[cruDF$lon == i & cruDF$lat == j, "ndviP"] <- ifelse(grid.check > 0, 
                                                                      out[out$lon == i & out$lat == j, "ndviP"],
                                                                      NA)
            
            cruDF[cruDF$lon == i & cruDF$lat == j, "ndviC"] <- ifelse(grid.check > 0, 
                                                                      out[out$lon == i & out$lat == j, "ndviC"],
                                                                      NA)
            cruDF[cruDF$lon == i & cruDF$lat == j, "ndviM"] <- ifelse(grid.check > 0, 
                                                                      out[out$lon == i & out$lat == j, "ndviM"],
                                                                      NA)
            cruDF[cruDF$lon == i & cruDF$lat == j, "ndviMean"] <- ifelse(grid.check > 0, 
                                                                         out[out$lon == i & out$lat == j, "ndviMean"],
                                                                         NA)
        }
    }
    
    write.table(cruDF, outPath, col.names=T, row.names=F, sep=",")
    
}

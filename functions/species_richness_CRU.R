####################################################################################
## Process species richness data onto CRU grids

richnessCRU <- function(inFile, outFile) {
    # Species richness data source: http://www.biodiversitymapping.org/download.htm  
    
    # library
    require(raster)
    require(rgdal)
    
    # read in CRU data and extract grid information
    cruDF <- read.table(paste(destDir, "/biome_temp_prec_full.csv", sep=""), 
                        header=T,sep=",")
    corDF <- data.frame(cruDF$lon, cruDF$lat, cruDF$CRU_Site)
    colnames(corDF) <- c("lon", "lat", "CRU_Site")
    
    # prepare output dataframe
    outDF <- corDF
    
    # convert cru into raster
    coordinates(corDF) <- ~lon+lat
    gridded(corDF) <- T
    cru <- raster(corDF)
    
    # prepare species class list
    class.list <- c("Amphibians", "Birds", "ConeSnails", "Mammals")
    class.short <- c("A_", "B_", "C_", "M_")
    
    ## Read in species richness data
    for (c.name in 1:length(class.list)) {
        
        # prepare directory paths
        sourceDir <- paste(inFile, "/", class.list[c.name],sep="")
        DatFiles <- list.files(path = sourceDir, pattern = "\\.tif")
        
        for (thisFile in 1:length(DatFiles)) {
            inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
            
            # prepare colname
            sub1 <- substr(DatFiles[thisFile], 15, nchar(DatFiles[thisFile]))
            sub2 <- substr(sub1,1,nchar(sub1)-11)
            colname <- paste(class.short[c.name], sub2, sep="")
            
            # read in raster file
            r <-raster(inName)
            
            # convert to points
            spts <- rasterToPoints(r, spatial = TRUE)
            
            # make projections and transformation
            llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
            llpts <- spTransform(spts, CRS(llprj))
            
            # save as data frame
            pnts <- as.data.frame(llpts)
            
            # rasterize the point data
            e <- extent(pnts[,2:3])
            ra <- raster(e, ncol=3329, nrow=1300)
            newR <- rasterize(pnts[,2:3], ra, pnts[,1], fun=mean)
            
            # resample species richness data based on CRU dataframe
            res <- resample(newR, cru, method="ngb")
            out <- as.data.frame(rasterToPoints(res))
            
            # prepare lon lat list to project out DF onto CRU DF
            lon.list <- unique(out$x)
            lat.list <- unique(out$y)
            for (i in lon.list) {
                for (j in lat.list) {
                    grid.check <- length(out[out$x == i & out$y == j, "layer"])
                    
                    outDF[outDF$lon == i & outDF$lat == j, colname] <- ifelse(grid.check > 0, 
                                                                              out[out$x == i & out$y == j, "layer"],
                                                                              NA)
                } ## for i list 
            }  # for j list
            
        } ## for thisFile list
    } ## species class list
    
    write.table(outDF, outFile, col.names=T, row.names=F, sep=",")
}
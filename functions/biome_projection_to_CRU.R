####################################################################################
## Project Biome onto CRU PCM data
biomeProject <- function(corFile, tempFile, precFile, pcmFile) {
    
    # read cor file
    corDF <- read.table(corFile, header=T,sep=",")
    
    # temperature PCM with BIOME
    tempPCM <- read.table(tempFile, header=T,sep=",")
    tempDF <- cbind(tempPCM, corDF$BIOME)
    
    colnames(tempDF) <- c("CRU_Site","lon","lat","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t",
                          "P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_Freedom","GP","P_freedom",
                          "BIOME")
    
    # precipitation PCM with BIOME
    precPCM <- read.table(precFile, header=T,sep=",")
    precDF <- cbind(precPCM, corDF$BIOME)
    
    colnames(precDF) <- c("CRU_Site","lon","lat","year","year_count","seasons",
                          "HofX","HofY","HofXY","HXofY","s","t",
                          "P","C","M","CbyP","MbyP",
                          "Mutual","GC","C_freedom","GM","M_Freedom","GP","P_freedom",
                          "BIOME")
    
    myDF <- cbind(tempDF$CRU_Site, tempDF$lon, tempDF$lat, 
                  tempDF$P, tempDF$C, tempDF$M, 
                  corDF$BIOME,
                  precDF$P, precDF$C, precDF$M)
    colnames(myDF) <- c("CRU_Site", "lon", "lat",
                        "tempP", "tempC", "tempM",
                        "BIOME",
                        "precP","precC","precM")
    myDF <- as.data.frame(myDF, stringsASfactors=F)
    
    write.table(myDF, pcmFile, sep=",",
                row.names=F, col.names=T)
    
}
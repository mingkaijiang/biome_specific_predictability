

####################################################################################
Australia_process_map <- function(inDF) {
    
    # Visually checking
    #with(inDF, quilt.plot(lon, lat, temp, 
    #                        xlim=c(110, 155), ylim=c(-45, -12), 
    #                        nx=720, ny=280))
    
    # read in file
    myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
    
    # looking for biome list from Australia 
    biome.list <- unique(myDF$BIOME)
    biome.list <- biome.list[-1]
    
    set.panel()
    par(oma=c(1,1,2,2),
        mar=c(5,4,4,5),
        mgp = c(3, 1, 0))  
    set.panel(2,2)
    
    # look for climate niche for each biome
    for (i in c(4,7,8,12,13)) {
        myDF1 <- subset(myDF, BIOME == i)
        globDF <- subset(inDF, BIOME == i)
        
        # looking for similar grids based on temp min and max
        temp.min <- min(myDF1$temp)
        temp.max <- max(myDF1$temp)
        subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
        dim(subDF.temp)
        
        # looking for similar grids based on additional prec min and max
        prec.min <- min(myDF1$prec_sum)
        prec.max <- max(myDF1$prec_sum)
        subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
        dim(subDF.prec)
        
        # Plot the selection in maps
        
        # temperature criteria
        with(subDF.prec, quilt.plot(lon, lat, temp, main = biome[i],
                                    xlab = "Temperature",cex.lab=2, cex.main=2,
                                    xlim=c(-180, 180), ylim=c(-60, 80),
                                    nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.8))
        
        # precipitation criteria
        with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                                    xlab = "Precipitation",cex.lab=2, cex.main=2,
                                    xlim=c(-180, 180), ylim=c(-60, 80),
                                    nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.8))
        
        # temperature P criteria
        with(subDF.prec, quilt.plot(lon, lat, tempP,cex.lab=2, cex.main=2,
                                    xlab = "Temperature predictability",
                                    xlim=c(-180, 180), ylim=c(-60, 80),
                                    nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.8))
        
        # precipitation P criteria
        with(subDF.prec, quilt.plot(lon, lat, precP,cex.lab=2, cex.main=2,
                                    xlab = "Precipitation predictability",
                                    xlim=c(-180, 180), ylim=c(-60, 80),
                                    nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.8))
    }
    
    #return(subDF.prec)
    
}

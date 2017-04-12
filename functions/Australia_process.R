
####################################################################################
Australia_process <- function(inDF) {
    
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
    set.panel(3,2)
    
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
        
        # looking for similar grids based on additional tempP min and max
        tempP.min <- min(myDF1$tempP)
        tempP.max <- max(myDF1$tempP)
        subDF.tempP <- subset(subDF.prec, tempP >= tempP.min & tempP <= tempP.max)
        dim(subDF.tempP)
        
        # looking for similar grids based on additional precP min and max
        precP.min <- min(myDF1$precP)
        precP.max <- max(myDF1$precP)
        subDF.precP <- subset(subDF.tempP, precP >= precP.min & precP <= precP.max)
        dim(subDF.precP)
        
        # Plot the selection in maps
        # setting graphics
        
        # temperature criteria
        with(inDF[inDF$BIOME == i, ], quilt.plot(lon, lat, temp, main = biome[i],
                                                 xlab = "Temperature",cex.lab=2, cex.main=2,
                                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                                 nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.5))
        
        # precipitation criteria
        with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                                    xlab = "Precipitation",cex.lab=2, cex.main=2,
                                    xlim=c(-180, 180), ylim=c(-60, 80),
                                    nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.5))
        
        # temperature P criteria
        with(subDF.tempP, quilt.plot(lon, lat, tempP,cex.lab=2, cex.main=2,
                                     xlab = "Temperature predictability",
                                     xlim=c(-180, 180), ylim=c(-60, 80),
                                     nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.5))
        
        # precipitation P criteria
        with(subDF.precP, quilt.plot(lon, lat, precP,cex.lab=2, cex.main=2,
                                     xlab = "Precipitation predictability",
                                     xlim=c(-180, 180), ylim=c(-60, 80),
                                     nx=400, ny=120))
        world(add=T, col=adjustcolor("grey", 0.5))
        
        # plot temp IE
        with(subDF.precP, quilt.plot(lon, lat, tempIE,col=c("red", "blue"),
                                     xlab = "Temperature Ie",cex.lab=2, cex.main=2,
                                     xlim=c(-180, 180), ylim=c(-60, 80),
                                     nx=680, ny=120), add.legend=F)
        legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"),
               cex=2)
        world(add=T, col=adjustcolor("grey", 0.5))
        
        # plot prec IE
        with(subDF.precP, quilt.plot(lon, lat, precIE,col=c("red", "blue"),
                                     xlab = "Precipitation Ie",cex.lab=2, cex.main=2,
                                     xlim=c(-180, 180), ylim=c(-60, 80),
                                     nx=680, ny=120), add.legend=F)
        legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"),
               cex=2)
        world(add=T, col=adjustcolor("grey", 0.5))
        
        
    }
    
    #return(subDF.precP)
    
}
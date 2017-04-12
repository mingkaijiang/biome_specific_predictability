####################################################################################
Tasmania_process <- function(inDF) {
    
    # Visually checking
    #with(inDF, quilt.plot(lon, lat, temp, 
    #                        xlim=c(140, 150), ylim=c(-45, -40), 
    #                        nx=720, ny=280))
    
    # read in file
    myDF <- subset(inDF, lon <= 150 & lon >= 140 & lat <= -40 & lat >= -45)
    
    # looking for similar grids based on temp min and max
    temp.min <- min(myDF$temp)
    temp.max <- max(myDF$temp)
    subDF.temp <- subset(inDF, temp >= temp.min & temp <= temp.max)
    dim(subDF.temp)
    
    # looking for similar grids based on additional prec min and max
    prec.min <- min(myDF$prec_sum)
    prec.max <- max(myDF$prec_sum)
    subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
    dim(subDF.prec)
    
    # looking for similar grids based on additional tempP min and max
    tempP.min <- min(myDF$tempP)
    tempP.max <- max(myDF$tempP)
    subDF.tempP <- subset(subDF.prec, tempP >= tempP.min & tempP <= tempP.max)
    dim(subDF.tempP)
    
    # looking for similar grids based on additional precP min and max
    precP.min <- min(myDF$precP)
    precP.max <- max(myDF$precP)
    subDF.precP <- subset(subDF.tempP, precP >= precP.min & precP <= precP.max)
    dim(subDF.precP)
    
    # Plot the selection in maps
    
    # temperature criteria
    with(subDF.temp, quilt.plot(lon, lat, temp, 
                                main = "Temperature",
                                xlim=c(-180, 180), ylim=c(-60, 80),
                                nx=680, ny=220))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation criteria
    with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                                main = "Precipitation",
                                xlim=c(-180, 180), ylim=c(-60, 80),
                                nx=680, ny=220))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # temperature P criteria
    with(subDF.tempP, quilt.plot(lon, lat, tempP,
                                 main = "Temperature predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=220))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation P criteria
    with(subDF.precP, quilt.plot(lon, lat, precP,
                                 main = "Precipitation predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=220))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # plot temp IE
    with(subDF.precP, quilt.plot(lon, lat, tempIE,col=c("red", "blue"),
                                 main = "Temperature Ie",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=220), add.legend=F)
    legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # plot prec IE
    with(subDF.precP, quilt.plot(lon, lat, precIE,col=c("red", "blue"),
                                 main = "Precipitation Ie",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=220), add.legend=F)
    legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    #return(subDF.precP)
    
}

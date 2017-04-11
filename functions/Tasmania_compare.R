
####################################################################################
Tasmania_compare <- function(inDF) {
    
    # read in file
    myDF <- subset(inDF, lon <= 150 & lon >= 140 & lat <= -40 & lat >= -45)
    myDF$lab <- "Tas"
    
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
    
    #  # visually check for regions to subset
    #  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
    #                              main = "Precipitation",
    #                              xlim=c(-180, 180), ylim=c(-60, 80),
    #                              nx=680, ny=220))
    #  world(add=T, col=adjustcolor("grey", 0.5))
    
    # Tasmania
    with(subDF.prec, quilt.plot(lon, lat, tempP, main="Tasmania",
                                xlim=c(140, 150), ylim=c(-45, -40), 
                                nx=700, ny=200))
    world(add=T)
    
    # EAsia
    with(subDF.prec, quilt.plot(lon, lat, tempP, main="East Asia",
                                xlim=c(80, 160), ylim=c(20, 50), 
                                nx=700, ny=200))
    world(add=T)
    
    # EU
    with(subDF.prec, quilt.plot(lon, lat, tempP, main="Europe",
                                xlim=c(-20, 50), ylim=c(30, 70), 
                                nx=700, ny=200))
    world(add=T)
    
    # NZ
    with(subDF.prec, quilt.plot(lon, lat, tempP, main="New Zealand",
                                xlim=c(160, 180), ylim=c(-60, -30), 
                                nx=700, ny=200))
    world(add=T)
    
    # US
    with(subDF.prec, quilt.plot(lon, lat, tempP, main="United States",
                                xlim=c(-120, -50), ylim=c(30, 70), 
                                nx=700, ny=200))
    world(add=T)
    
    # Subsetting different regions
    
    # New Zealand
    NZ <- subset(subDF.prec, lon <= 180 & lon >= 160 & lat <= -30 & lat >= -60)
    NZ$lab <- "NZ"
    # Eastern Asia
    EA <- subset(subDF.prec, lon <= 160 & lon >= 80 & lat <= 50 & lat >= 20)
    EA$lab <- "EAsia"
    # Europe
    EU <- subset(subDF.prec, lon <= 50 & lon >= -20 & lat <= 70 & lat >= 30)
    EU$lab <- "EU"
    # US
    US <- subset(subDF.prec, lon <= -50 & lon >= -120 & lat <= 70 & lat >= 30)
    US$lab <- "US"
    
    finalDF <- rbind(myDF, NZ, EA, EU, US)
    
    # Plotting
    set.panel()
    #par(oma=c(2,2,2,2),
    #    mgp = c(2, 1, 0))
    set.panel(2,1)
    
    # Temperature predictability
    with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                          ylab = "Temperature predictability",
                          col=c("blue","red","yellow","cyan","orange"),
                          cex.lab=1.2, cex.axis=1.2))
    
    # Precipitation predictability
    with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                          ylab = "Precipitation predictability",
                          col=c("blue","red","yellow","cyan","orange"),
                          cex.lab=1.2, cex.axis=1.2))
    
}


####################################################################################
Australia_compare <- function(inDF) {
    
    # read in file
    myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
    myDF1 <- subset(myDF, BIOME == 4)
    globDF <- subset(inDF, BIOME == 4)
    
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
    
    #  # visually check
    #  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
    #                              xlab = "Precipitation",cex.lab=2, cex.main=2,
    #                              xlim=c(-180, 180), ylim=c(-60, 80),
    #                              nx=400, ny=120))
    #  world(add=T, col=adjustcolor("grey", 0.5))
    
    # map plot
    # Australia
    with(subDF.prec, quilt.plot(lon, lat, precP, 
                                main = "Australia",
                                xlim=c(110, 155), ylim=c(-45, -12),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("black", 1))
    
    # China
    with(subDF.prec, quilt.plot(lon, lat, precP, 
                                main = "China",
                                xlim=c(80, 160), ylim=c(20, 50),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("black", 1))
    
    # EU
    with(subDF.prec, quilt.plot(lon, lat, precP, 
                                main = "Europe",
                                xlim=c(-20, 50), ylim=c(30, 70),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("black", 1))
    
    # US
    with(subDF.prec, quilt.plot(lon, lat, precP, 
                                main = "United States",
                                xlim=c(-120, -50), ylim=c(30, 70),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("black", 1))
    
    # Subsetting different regions
    
    # Oz
    AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
    AU$lab <- "AU"
    # China
    CH <- subset(subDF.prec, lon <= 160 & lon >= 80 & lat <= 50 & lat >= 20)
    CH$lab <- "CH"
    # Europe
    EU <- subset(subDF.prec, lon <= 50 & lon >= -20 & lat <= 70 & lat >= 30)
    EU$lab <- "EU"
    # US
    US <- subset(subDF.prec, lon <= -50 & lon >= -120 & lat <= 70 & lat >= 30)
    US$lab <- "US"
    
    finalDF <- rbind(AU, CH, EU, US)
    
    # Plotting
    set.panel()
    #par(oma=c(2,2,2,2),
    #    mgp = c(2, 1, 0))
    set.panel(2,1)
    
    # Temperature predictability
    with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                          ylab = "Temperature predictability",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
    
    # Precipitation predictability
    with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                          ylab = "Precipitation predictability",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
}
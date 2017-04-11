
####################################################################################
Australia_compare_8 <- function(inDF) {
    
    #library
    require(agricolae)
    
    # read in file
    myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
    myDF1 <- subset(myDF, BIOME == 8)
    globDF <- subset(inDF, BIOME == 8)
    
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
    #with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
    #                            xlab = "Precipitation",cex.lab=2, cex.main=2,
    #                            xlim=c(-180, 180), ylim=c(-90, 90),
    #                            nx=400, ny=120))
    #world(add=T, col=adjustcolor("grey", 0.5))
    
    # Subsetting different regions
    
    # Oz
    AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= 0 & lat >= -45)
    AU$lab <- "AU"
    # US
    US <- subset(subDF.prec, lon <= -50 & lon >= -150 & lat <= 50 & lat >= 0)
    US$lab <- "US"
    # South America
    SA <- subset(subDF.prec, lon <= -50 & lon >= -100 & lat <= 0 & lat >= -50)
    SA$lab <- "SA"
    
    finalDF <- rbind(AU, US, SA)
    finalDF$lab <- factor(finalDF$lab)
    
    # statistical test
    mod1 <- lm(tempP~lab, data=finalDF)
    summary(mod1)
    anova(mod1)
    confint(mod1)
    aov.out1 <- aov(tempP~lab, data=finalDF)
    out1 <- HSD.test(aov.out1, "lab")
    
    aov.out2 <- aov(precP~lab, data=finalDF)
    out2 <- HSD.test(aov.out2, "lab")
    
    # Plotting
    set.panel()
    set.panel(2,1)
    
    # Temperature predictability
    with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                          ylab = "Temperature predictability",
                          col=c("blue","red","yellow"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.22, line = -1.6, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.50, line = -3.2, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.82, line = -2.5, cex = 1.5, ft=2)
    
    # Precipitation predictability
    with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                          ylab = "Precipitation predictability",
                          col=c("blue","red","yellow"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.2, line = -1.6, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.54, line = -1.4, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.85, line = -1.2, cex = 1.5, ft=2)
}
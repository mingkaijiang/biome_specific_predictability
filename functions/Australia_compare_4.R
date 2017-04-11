####################################################################################
Australia_compare_4 <- function(inDF) {
    
    #library
    require(agricolae)
    
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
    finalDF$lab <- factor(finalDF$lab)
    
    # statistical test
    #mod1 <- lm(tempP~lab, data=finalDF)
    #summary(mod1)
    #anova(mod1)
    #confint(mod1)
    aov.out1 <- aov(tempP~lab, data=finalDF)
    out1 <- HSD.test(aov.out1, "lab")
    
    aov.out2 <- aov(precP~lab, data=finalDF)
    out2 <- HSD.test(aov.out2, "lab")
    
    aov.out3 <- aov(precC~lab, data=finalDF)
    out3 <- HSD.test(aov.out3, "lab")
    
    aov.out4 <- aov(precM~lab, data=finalDF)
    out4 <- HSD.test(aov.out4, "lab")
    
    # Plotting
    set.panel()
    set.panel(2,2)
    
    # Temperature predictability
    with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                          ylab = "Temperature predictability",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
    mtext("a", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.62, line = -2, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.85, line = -2.2, cex = 1.5, ft=2)
    
    # Precipitation predictability
    with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                          ylab = "Precipitation predictability",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
    mtext("d", side = 3, adj = 0.85, line = -1.2, cex = 1.5, ft=2)
    
    # Precipitation constancy
    with(finalDF, boxplot(precC ~ lab, xlab = "Regions", 
                          ylab = "Precipitation constancy",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
    mtext("d", side = 3, adj = 0.88, line = -2, cex = 1.5, ft=2)
    
    # Precipitation contingency
    with(finalDF, boxplot(precM ~ lab, xlab = "Regions", 
                          ylab = "Precipitation contingency",
                          col=c("blue","red","yellow","cyan"),
                          cex.lab=1.2, cex.axis=1.2))
    mtext("a", side = 3, adj = 0.18, line = -8, cex = 1.5, ft=2)
    mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
    mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
    mtext("d", side = 3, adj = 0.85, line = -7.5, cex = 1.5, ft=2)
}
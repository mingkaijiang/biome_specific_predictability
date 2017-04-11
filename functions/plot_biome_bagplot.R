####################################################################################
## plotting bagplot of all data range
biomeBagPlot <- function(inDF) {
    ## Plotting biome-specific 2d graphs for:
    ## 1. temperature vs. precipitation
    ## 2. temperature predictability vs. precipitation predictability
    
    # prepare dataframe
    plotDF2 <- subset(inDF, BIOME > 0)
    plotDF2 <- subset(plotDF2, BIOME < 98)
    
    # Set output graph structure
    set.panel()
    par(oma=c(2,4,2,2),
        mar=c(5.1,5.1,4.1,1.2),
        mgp = c(3, 1, 0))
    set.panel(5,3)
    
    ## Plot temp vs. prec for each biome
    for (i in 1:14) {
        DF <- subset(plotDF2, BIOME == i)
        bagplot(DF$temp, DF$prec_sum, 
                ylab = "Precipitation", xlab = "Temperature", cex.lab=2, 
                xlim = c(-30,40), ylim = c(0,8000),
                main = paste(biome[i]), cex.main=2, show.whiskers=F)
    }
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    
    ## Plot temp P vs. prec P for each biome
    for (i in 1:14) {
        DF <- subset(plotDF2, BIOME == i)
        bagplot(DF$tempP, DF$precP, 
                ylab = "Precipitation predictability", xlab = "Temperature predictability", cex.lab=2, 
                xlim = c(0,1), ylim = c(0,1),
                main = paste(biome[i]), cex.main=2, show.whiskers=F)
    }
    par(opar)
}

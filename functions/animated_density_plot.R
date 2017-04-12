
####################################################################################
## plotting animated density plot for climate
AnimatedDensityPlot <- function(inDF) {
    ## Plotting biome-specific density graphs for:
    ## 1. temperature vs. precipitation
    ## 2. temperature predictability vs. precipitation predictability
    
    # library
    #require(vegan)
    #require(ks)
    
    # prepare dataframe
    plotDF2 <- subset(inDF, BIOME > 0)
    plotDF2 <- subset(plotDF2, BIOME < 98)
    
    # set frames
    frames = 14
    
    ## Plot temp vs. prec for each biome
    for (i in 1:frames) {
        
        # creating a name for each plot file with leading zeros
        if (i < 10) {name = paste('000',i,'plot.png',sep='')}
        if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
        
        png(name, width=960, height=480)
        
        # Set output graph structure
        set.panel()
        par(oma=c(2,4,2,2),
            mar=c(5.1,5.1,4.1,1.2),
            mgp = c(3, 1, 0))
        layout(matrix(c(1,2),1,2))
        
        # subsetting dataframe
        DF <- data.frame(plotDF2[plotDF2$BIOME == i, "temp"],
                         plotDF2[plotDF2$BIOME == i, "prec_sum"])
        # kernel density estimation
        H <- Hpi(x=DF)      # optimal bandwidth estimation
        est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
        
        # set contour probabilities for drawing contour levels
        cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
        
        plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
             ylab="Precipitation", xlab="Temperature", main=biome[i],
             ylim=c(0,8000), xlim=c(-30,40),las=1, cex.lab=2, cex.main=2) 
        plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
        plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        
        # subsetting dataframe
        DF <- data.frame(plotDF2[plotDF2$BIOME == i, "tempP"],
                         plotDF2[plotDF2$BIOME == i, "precP"])
        # kernel density estimation
        H <- Hpi(x=DF)      # optimal bandwidth estimation
        est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
        
        # set contour probabilities for drawing contour levels
        cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
        
        plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
             ylab="Precipitation predictability", xlab="Temperature predictability", main=biome[i],
             cex.lab=2, cex.main=2, ylim=c(0,1), xlim=c(0,1),las=1) 
        plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
        plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        
        dev.off()
        
        par(opar)
    }
}

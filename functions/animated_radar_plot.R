####################################################################################
## plotting animated density plot for climate
Animated_radar <- function(inDF) {
    ## Plotting biome-specific density graphs for:
    ## 1. temperature vs. precipitation
    ## 2. temperature predictability vs. precipitation predictability
    
    # library
    require(fmsb)
    
    # prepare dataframe
    col.list <- rainbow(14)
    newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                        inDF$tempP_mean,inDF$precP_mean)
    names(newDF) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF) <- biome
    
    # set frames
    frames = 14
    
    ## Plot temp vs. prec for each biome
    for (i in 1:frames) {
        
        # creating a name for each plot file with leading zeros
        if (i < 10) {name = paste('000',i,'plot.png',sep='')}
        if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
        
        png(name, width=960, height=480)
        plotDF <- rbind(c(30, 2500, 1, 1), c(-20, 0, 0, 0), newDF[i,])
        radarchart(plotDF, pcol=col.list[i], 
                   vlcex=2, title=biome[i],cex.main=2,
                   pfcol=adjustcolor(col.list[i],0.2))
        # title(biome[i], cex.lab=2)
        
        dev.off()
        
        par(opar)
    }
    
}

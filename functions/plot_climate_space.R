####################################################################################
## Plot climate space using hexagon   -- NOT USED
ClimSpace <- function(inDF) {
    
    # library
    require(hexbin)
    
    # read in file
    newDF <- inDF
    
    # temperature vs. precipitation
    hbin <- hexbin(newDF$temp, newDF$prec_sum, xbins=40,
                   xlab="temperature", ylab="precipitation")
    plot(hbin)
    
    # temperature pred vs. precipitation pred
    hbin <- hexbin(newDF$tempP, newDF$precP, 
                   xlab="temperature predictability",
                   ylab="precipitation predictability", xbins=40)
    plot(hbin)
    
    # temperature vs. temp pred
    hbin <- hexbin(newDF$temp_annual_mean, newDF$tempP, 
                   xlab="temperature",
                   ylab="temperature predictability", xbins=40)
    plot(hbin)
    
    # prec vs. prec pred
    hbin <- hexbin(newDF$prec_annual_sum, newDF$precP, 
                   xlab="precipitation",
                   ylab="precipitation predictability", xbins=40)
    p<-plot(hbin)
    pushHexport(p$plot.vp)
    
    # add points
    grid.points(1000, 0.4, pch=16, gp=gpar(col="red"))
    upViewport()
    
}

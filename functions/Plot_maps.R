####################################################################################
## Plotting function 1
PlotMaps <- function(inDF) {
    ## Plot the following maps:
    ## 1. global temperature profile map
    ## 2. global temperature predictability map
    ## 3. global temperature constancy map
    ## 4. global temperautre contigency map
    ## 5. temperature vs. temperature predictability map, grouped by IE factor
    ## 6. Violin plot of the latitudinal patterns of Ie factor
    
    ##### temperature 
    
    # plot temperature mean map
    par(oma=c(1,1,2,2),
        mar=c(5.1,2,4,5))
    with(inDF, quilt.plot(lon, lat, templab, nx=300, ny=260, 
                          main="Annual mean temperature", add.legend=F))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = temp.lab, title = expression(paste("Temperature [",degree,"C]")),
           fill=tim.colors(12), cex=1, bty="n")
    par(opar)
    
    # plot temperature predictability map
    with(inDF, quilt.plot(lon, lat, tempP, nx=300, ny=260, nlevel=12,
                          main="Temperature predictability", add.legend=T))
    
    # plot temperature constancy map
    with(inDF, quilt.plot(lon, lat, tempC, nx=300, ny=260, nlevel=12,
                          main="Temperature constancy", add.legend=T))
    
    # plot temperature contingency map
    with(inDF, quilt.plot(lon, lat, tempM, nx=300, ny=260, nlevel=12,
                          main="Temperature contingency", add.legend=T))
    
    # plot temperature vs. temperature predictability, grouped by IE factor
    par(oma=c(1,1,1,2),
        mar=c(5,5,4,2))
    with(inDF[inDF$tempIE == 1, ], plot(temp, tempP, 
                                        xlab = expression(paste("Temperature [",degree,"C]")),
                                        ylab = "Temperature predictability",
                                        col=adjustcolor("darkgreen", 0.5),
                                        cex=0.2))
    with(inDF[inDF$tempIE == 2, ], points(temp, tempP,
                                          xlab=NA, ylab=NA, 
                                          col=adjustcolor("lightgreen", 0.5),
                                          cex=0.2))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = c("> 1", "< 1"), title = "Ie factor",
           fill=c("darkgreen", "lightgreen"), cex=1, bty="n")
    par(opar)
    
    # plot Violin plot for latitudinal gradient of temperature Ie and pred
    vioplot(inDF$lat[inDF$tempIE == 1], 
            inDF$lat[inDF$tempIE == 2], names = c("Ie < 1", "Ie > 1"),
            col = "gold")
    title(ylab="Latitude")
    
    ##### precipitation
    
    # plot precipitation map
    par(oma=c(1,1,2,2),
        mar=c(5.1,2,4,5))
    with(inDF, quilt.plot(lon, lat, preclab, nrow=300, ncol=260, 
                          main="Annual sum precipitation", add.legend=F))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = prec.lab, title = "Precipitation [mm]",
           fill=tim.colors(12), cex=1, bty="n")
    par(opar)
    
    # plot precipitation predictability map
    with(inDF, quilt.plot(lon, lat, precP, nx=300, ny=260, nlevel=12,
                          main="Precipitation predictability", add.legend=T))
    
    # plot Precipitation constancy map
    with(inDF, quilt.plot(lon, lat, precC, nx=300, ny=260, nlevel=12,
                          main="Precipitation constancy", add.legend=T))
    
    # plot Precipitation contingency map
    with(inDF, quilt.plot(lon, lat, precM, nx=300, ny=260, nlevel=12,
                          main="Precipitation contingency", add.legend=T))
    
    # plot Precipitation vs. Precipitation predictability, grouped by IE factor
    par(oma=c(1,1,1,2),
        mar=c(5,5,4,2))
    with(inDF[inDF$precIE == 1, ], plot(prec_sum, precP, 
                                        xlab = "Precipitation",
                                        ylab = "Precipitation predictability",
                                        col=adjustcolor("darkgreen", 0.5),
                                        cex=0.2))
    with(inDF[inDF$precIE == 2, ], points(prec_sum, precP,
                                          xlab=NA, ylab=NA, 
                                          col=adjustcolor("lightgreen", 0.5),
                                          cex=0.2))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = c("> 1", "< 1"), title = "Ie factor",
           fill=c("darkgreen", "lightgreen"), cex=1, bty="n")
    par(opar)
    
    # plot Violin plot for latitudinal gradient of precipitation Ie and pred
    vioplot(inDF$lat[inDF$precIE == 1], 
            inDF$lat[inDF$precIE == 2], names = c("Ie < 1", "Ie > 1"),
            col = "gold")
    title(ylab="Latitude")
}

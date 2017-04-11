####################################################################################
# Plot Australia maps
Aus_Plot <- function(cru) {
    
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
        mar=c(2,2,2,5))
    with(cru, quilt.plot(lon, lat, templab, nx=720, ny=280, 
                         main="Annual mean temperature", xlim=c(110,160), ylim=c(-45,-5),
                         add.legend=F))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = temp.lab, title = expression(paste("Temperature [",degree,"C]")),
           fill=tim.colors(12), cex=1, bty="n")
    par(opar)
    
    # plot temperature predictability map
    with(cru, quilt.plot(lon, lat, tempP, nx=720, ny=280,  nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Temperature predictability", add.legend=T))
    
    # plot temperature constancy map
    with(cru, quilt.plot(lon, lat, tempC, nx=720, ny=280, nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Temperature constancy", add.legend=T))
    
    # plot temperature contingency map
    with(cru, quilt.plot(lon, lat, tempM, nx=720, ny=280,  nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Temperature contingency", add.legend=T))
    
    ##### precipitation
    
    # plot precipitation map
    par(oma=c(1,1,2,2),
        mar=c(2,2,2,5))
    with(cru, quilt.plot(lon, lat, preclab, nx=720, ny=280, 
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Annual sum precipitation", add.legend=F))
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = prec.lab, title = "Precipitation [mm]",
           fill=tim.colors(12), cex=1, bty="n")
    par(opar)
    
    # plot precipitation predictability map
    with(cru, quilt.plot(lon, lat, precP, nx=720, ny=280,  nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Precipitation predictability", add.legend=T))
    
    # plot Precipitation constancy map
    with(cru, quilt.plot(lon, lat, precC, nx=720, ny=280,  nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Precipitation constancy", add.legend=T))
    
    # plot Precipitation contingency map
    with(cru, quilt.plot(lon, lat, precM, nx=720, ny=280,  nlevel=12,
                         xlim=c(110,160), ylim=c(-45,-5),
                         main="Precipitation contingency", add.legend=T))
    
}

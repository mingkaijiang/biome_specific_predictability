####################################################################################
## Plotting function 1
plot_PCM_maps <- function(inDF) {
    ## Plot the following maps:
    ## 1. global temperature predictability map
    ## 2. global temperature constancy map
    ## 3. global temperautre contigency map
    ## 4. global prec predictability map
    ## 5. global prec constancy map
    ## 6. global prec contigency map
    
    m <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2,byrow=T)
    layout(mat=m, heights=c(0.33, 0.33, 0.33))
    
    par(mar=c(2.1,4.1,4.1,2))
    
    ##### temperature 
    # plot temperature predictability map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, tempP, nx=300, ny=260, nlevel=12,
                    main="Temperature", add.legend=T,
                    ylab = "Predictability", cex.lab=1.5,
                    cex.main=1.5))
    mtext("(a)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
    # plot precipitation predictability map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, precP, nx=300, ny=260, nlevel=12,
                    main="Precipitation", add.legend=T, cex.lab=1.5,
                    cex.main=1.5))
    mtext("(b)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
    # plot temperature constancy map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, tempC, nx=300, ny=260, nlevel=12,
                    add.legend=T, ylab = "Constancy", cex.lab=1.5,
                    cex.main=1.5))
    mtext("(c)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
    # plot Precipitation constancy map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, precC, nx=300, ny=260, nlevel=12,
                    add.legend=T, cex.lab=1.5,
                    cex.main=1.5))
    mtext("(d)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
    # plot temperature contingency map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, tempM, nx=300, ny=260, nlevel=12,
                    add.legend=T, ylab = "Contingency", cex.lab=1.5,
                    cex.main=1.5))
    mtext("(e)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
    # plot Precipitation contingency map
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
         quilt.plot(lon, lat, precM, nx=300, ny=260, nlevel=12,
                    add.legend=T, cex.lab=1.5,
                    cex.main=1.5))
    mtext("(f)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
    
}

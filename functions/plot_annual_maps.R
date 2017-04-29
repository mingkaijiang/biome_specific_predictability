####################################################################################
## Plotting all annual climate
plot_maps_annual <- function(inDF) {
    ## Plot the following maps:
    ## 1. global MAT
    ## 2. global ATP
    
    split.screen(c(2,1))
    
    screen(1)
    # plot temperature mean map
    par(oma=c(1,1,2,2),
        mar=c(3,2,2,5))
    with(inDF, quilt.plot(lon, lat, templab, nx=300, ny=260, 
                          main="Annual mean temperature", add.legend=F))
    mtext("(a)", side = 3, adj = 0.05, line = -7.5, cex = 1.5)
    
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend(0.7, 0.8, legend = temp.lab, 
           title = expression(paste("Temperature [",degree,"C]")),
           fill=tim.colors(12), cex=0.7, bty="n")
    par(opar)
    
    screen(2)
    # plot precipitation map
    par(oma=c(1,1,2,2),
        mar=c(1,2,3,5))
    with(inDF, quilt.plot(lon, lat, preclab, nrow=300, ncol=260, 
                          main="Annual total precipitation", add.legend=F))
    mtext("(b)", side = 3, adj = 0.05, line = -8.5, cex = 1.5)
    
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend(0.7,-0.2, legend = prec.lab, title = "Precipitation [mm]",
           fill=tim.colors(12), cex=0.7, bty="n")
    par(opar)
    
    close.screen(all = T)
}

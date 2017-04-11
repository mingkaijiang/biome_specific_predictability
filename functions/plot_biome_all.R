####################################################################################
## plotting temp vs. prec all data onto same graph
plot_biome_all <- function(inDF) {
    ## Plotting biome-specific 2d graphs for:
    ## 1. temperature vs. precipitation
    ## 2. temperature predictability vs. precipitation predictability
    
    # prepare dataframe
    plotDF2 <- subset(inDF, BIOME > 0)
    plotDF2 <- subset(plotDF2, BIOME < 98)
    
    # Setting color.list
    col.list <- palette(rainbow(14))
    
    # temp vs. prec
    par(oma=c(1,1,1,1),
        mar=c(5.1,5.1,4,12))
    with(plotDF2[plotDF$BIOME == 1, ], plot(temp, prec_sum, 
                                            xlab="Temperature", ylab="Precipitation",
                                            col=adjustcolor(col.list[1],0.5), xlim=c(-30,40), 
                                            ylim=c(0,8000), cex=0.2))
    for(i in 2:14) {
        with(plotDF2[plotDF$BIOME == i, ], points(temp, prec_sum, 
                                                  xlab=NA, ylab=NA,
                                                  col=adjustcolor(col.list[i],0.5), xlim=c(-30,40), 
                                                  ylim=c(0,8000), add=T, cex=0.2))
    }
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = biome, title = "Biome",
           fill=palette(rainbow(14)), cex=1, bty="n")
    par(opar)
    
    # temp P vs. prec P
    par(oma=c(1,1,1,1),
        mar=c(5.1,5.1,4,12))
    with(plotDF2[plotDF$BIOME == 1, ], plot(tempP, precP, 
                                            xlab="Temperature P", ylab="Precipitation P",
                                            col=adjustcolor(col.list[1],0.5), xlim=c(0,1), 
                                            ylim=c(0,1), cex=0.2))
    for(i in 2:14) {
        with(plotDF2[plotDF$BIOME == i, ], points(tempP, precP, 
                                                  xlab=NA, ylab=NA,
                                                  col=adjustcolor(col.list[i],0.5), xlim=c(0,1), 
                                                  ylim=c(0,1), add=T, cex=0.2))
    }
    par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    legend("right", legend = biome, title = "Biome",
           fill=palette(rainbow(14)), cex=1, bty="n")
    par(opar)
}

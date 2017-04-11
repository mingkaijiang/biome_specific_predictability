
####################################################################################
##Plot 2d environmental information factor for each biome
Ieplot <- function(inDF) {
    
    # library
    #require(lattice)
    #require(gridExtra)
    
    # read in file
    myDF <- inDF
    
    # process each biome
    for (i in 1:14) {
        # set plot structure
        m <- matrix(c(1,2,3), nrow=3, ncol=1, byrow=T)
        layout(mat=m, heights=c(0.4, 0.4, 0.2))
        
        # temperature vs. temperature predictability
        with(myDF[myDF$BIOME == i & myDF$tempIE == 1, ], plot(temp, tempP, xlab="Temperature",
                                                              ylab = "Temperature predictability",
                                                              main = biome[i], xlim = c(-40,30),
                                                              cex = 0.2, ylim = c(0,1),
                                                              col=adjustcolor("green", 0.5)))
        with(myDF[myDF$BIOME == i & myDF$tempIE == 2, ], points(temp, tempP, xlab=NA, ylab=NA,
                                                                xlim=c(-40,30),
                                                                ylim=c(0,1), cex = 0.2,
                                                                col=adjustcolor("blue", 0.5)))
        
        # precipitation vs. precipitation predictability
        with(myDF[myDF$BIOME == i & myDF$tempIE == 1, ], plot(prec_sum, precP, xlab="Precipitation",
                                                              ylab="Precipitation predictability",
                                                              main = biome[i], xlim=c(0,8000),
                                                              ylim=c(0,1), cex = 0.2,
                                                              col=adjustcolor("green", 0.5)))
        with(myDF[myDF$BIOME == i & myDF$tempIE == 2, ], points(prec_sum, precP, xlab="Precipitation",
                                                                ylab="Precipitation predictability",
                                                                xlim=c(0,8000),
                                                                ylim=c(0,1), cex = 0.2,
                                                                col=adjustcolor("blue", 0.5)))
        par(mar = rep(2, 4))
        plot(1, type="n", axes=F, xlab="", ylab="")
        legend(x="bottom",inset=0, legend = c("Ie < 1", "Ie > 1"), fill=c("green", "blue"), 
               cex=1.5, horiz=T)
        par(opar)
    }
    
}

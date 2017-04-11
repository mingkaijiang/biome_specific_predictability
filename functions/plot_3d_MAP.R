
####################################################################################
plot3d_MAP <- function(myDF) {
    # plot 3d with MAP, C and M for prec
    
    ## Explore 3-d plottings
    require(plot3D)
    
    ##Plot each biome separately
    with(myDF[myDF$BIOME == 1, ], scatter3D(precC, precM, prec_sum,
                                            xlim=c(0,1), ylim=c(0,1),
                                            zlim=c(0,8000), xlab="Constancy",
                                            ylab = "Contingency",
                                            zlab = "MAP",
                                            pch = 19, col=color.list[1]))
    for (i in 2: 14) {
        with(myDF[myDF$BIOME == i, ], points3D(precC, precM, prec_sum,
                                               pch = 19, col=color.list[i], add=T))
    }
    
}
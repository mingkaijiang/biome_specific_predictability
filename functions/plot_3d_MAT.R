
####################################################################################
plot3d_MAT <- function(myDF) {
    # plot 3d with MAT, C and M for temp
    
    ## Explore 3-d plottings
    require(plot3D)
    
    ##Plot each biome separately
    with(myDF[myDF$BIOME == 1, ], scatter3D(tempC, tempM, temp,
                                            xlim=c(0,1), ylim=c(0,1),
                                            zlim=c(-30,50), xlab="Constancy",
                                            ylab = "Contingency",
                                            zlab = "MAT",
                                            pch = 19, col=color.list[1]))
    for (i in 2: 14) {
        with(myDF[myDF$BIOME == i, ], points3D(tempC, tempM, temp,
                                               pch = 19, col=color.list[i], add=T))
    }
    
}
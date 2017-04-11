
####################################################################################
# Plot biome map
biome_plot <- function(inDF) {
    
    # setting graphics
    m <- matrix(c(1,2), nrow=2, ncol=1,byrow=T)
    layout(mat=m, heights=c(0.8,0.2))
    
    # plotting
    with(inDF[inDF$BIOME <= 14 & inDF$BIOME > 0, ], 
         quilt.plot(lon, lat, BIOME,
                    nx=300, ny=260, col=color.list,
                    add.legend=F))
    world(add=T)
    
    # legend
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
           cex=0.8,ncol=4)
    
    par(opar)
}


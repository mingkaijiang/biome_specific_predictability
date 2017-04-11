
####################################################################################
## kernel density plot
kernel_multi <- function(inDF) {
    
    newDF <- inDF
    # library
    require(sm)
    
    # set plot structure
    m <- matrix(c(1,2,3,4,5,5), nrow=3, ncol=2,byrow=T)
    layout(mat=m, heights=c(0.4, 0.4, 0.2))
    
    ## temperature
    sm.density.compare(newDF$temp, newDF$BIOME, xlab="temperature")
    
    ## precipitation
    sm.density.compare(newDF$prec_sum, newDF$BIOME, xlab="precipitation")
    
    ## temperature predictability
    sm.density.compare(newDF$tempP, newDF$BIOME, xlab="temperature predictability")
    
    ## precipitation predictability
    sm.density.compare(newDF$precP, newDF$BIOME, xlab="precipitation predictability")
    
    colfill<-c(2:15)
    
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biome, fill=colfill, 
           cex=0.8,ncol=4)
    par(opar)
    
}

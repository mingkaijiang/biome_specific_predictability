####################################################################################
## Plot radar plots
radar_summary_image2 <- function(inDF) {
    
    # library
    #require(fmsb)
    
    # prepare dataframe
    col.list <- color.list
    
    newDF2 <- data.frame(inDF$temp_mean, inDF$prec_mean,
                         inDF$tempP_mean,inDF$precP_mean)
    names(newDF2) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF2) <- biome
    
    op <- par(xpd=T,
              mfrow=c(5,3),
              mar=c(1,0.2,1.5,0.2))
    
    spider(y=1:14, x=1:4, data=newDF2, rescale=T, 
           main = biome, connect=T, ncol=1, fill=T)
    
    par(op)
}

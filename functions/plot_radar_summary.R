
####################################################################################
## Plot radar plots
radar_summary <- function(inDF) {
    
    # library
    #require(fmsb)
    
    # prepare dataframe
    col.list <- color.list
    newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                        inDF$tempP_mean,inDF$precP_mean)
    names(newDF) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF) <- biome
    
    # plotting
    for(i in 1:14) {
        plotDF <- rbind(c(30, 2500, 1, 1), c(-20, 0, 0, 0), newDF[i,])
        radarchart(plotDF, pcol=col.list[i], pfcol=adjustcolor(col.list[i],0.2))
        title(biome[i])
    }  
    
}
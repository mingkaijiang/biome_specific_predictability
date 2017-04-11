####################################################################################
## Plot radar plots
radar_summary_image <- function(inDF1, inDF2) {
    
    # library
    require(fmsb)
    
    # prepare dataframe
    col.list <- color.list
    newDF1 <- data.frame(inDF1$temp_max, inDF1$prec_max,
                         inDF1$tempP_max,inDF1$precP_max)
    names(newDF1) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF1) <- biome
    
    newDF2 <- data.frame(inDF2$temp_min, inDF2$prec_min,
                         inDF2$tempP_min,inDF2$precP_min)
    names(newDF2) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF2) <- biome
    
    par(mfrow=c(5,3))
    
    # plotting
    for(i in 1:14) {
        plotDF <- rbind(c(35, 7500, 1, 1), c(-30, 0, 0, 0), newDF1[i,], newDF2[i,])
        radarchart(plotDF, pcol=c(col.list[i],adjustcolor("white",0)),axistype=2,
                   pfcol=c(adjustcolor(col.list[i],0.2),adjustcolor("white",0.8)),
                   axislabcol="black", vlcex = 1.2, caxislabels = c(-30, 0, 0, 0),
                   paxislabels=c(35, 7500, 1, 1),cglwd=0.8)
        title(biome[i])
    }  
    par(opar)
}
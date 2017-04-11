####################################################################################
## Plot oz flux climate data
OzPlot <- function(inDF) {
    
    # color list NOTE: not to be confused with col.list in Main program
    col.list2 <- c("red", "green")
    
    # setting graphics
    par(oma=c(1,1,2,2),
        mar=c(2,10,2,2))
    
    
    # order with mean annual temperature
    inDF <- inDF[order(inDF$temp),]
    
    # Temperature PCM
    
    # create a subset of data table
    plotDF2 <- t(data.frame(inDF$SiteName, inDF$tempC, inDF$tempM))
    colnames(plotDF2) <- plotDF2[1,]
    plotDF2 <- plotDF2[-1,]
    rownames(plotDF2) <- c("C", "M")
    
    # bar plot
    barplot(plotDF2, horiz=T, col=col.list2,las=2,xlim=c(0,1.4),
            main = "Temperature predictability")
    
    legend("bottomright", c("C", "M"), bty="n", fill=col.list2)
    
    # temperature labels
    text(1.1, 24.8, inDF[21, "temp"])
    text(1.05, 23.6, inDF[20, "temp"])
    text(1.05, 22.4, inDF[19, "temp"])
    text(1.1, 21.2, inDF[18, "temp"])
    text(1.05, 20.0, inDF[17, "temp"])
    text(1.05, 18.7, inDF[16, "temp"])
    text(1.05, 17.6, inDF[15, "temp"])
    text(0.98, 16.4, inDF[14, "temp"])
    text(0.98, 15.2, inDF[13, "temp"])
    text(0.95, 14.0, inDF[12, "temp"])
    text(0.95, 12.7, inDF[11, "temp"])
    text(0.85, 11.5, inDF[10, "temp"])
    text(0.85, 10.3, inDF[9, "temp"])
    text(0.85, 9.0, inDF[8, "temp"])
    text(0.85, 7.8, inDF[7, "temp"])
    text(0.85, 6.6, inDF[6, "temp"])
    text(0.85, 5.4, inDF[5, "temp"])
    text(0.85, 4.2, inDF[4, "temp"])
    text(0.85, 3, inDF[3, "temp"])
    text(0.85, 1.8, inDF[2, "temp"])
    text(0.85, 0.6, inDF[1, "temp"])
    
    # Precipitation PCM
    # order with precipitation totals
    inDF <- inDF[order(inDF$prec_sum),]
    # create a subset of data table
    plotDF2 <- t(data.frame(inDF$SiteName, inDF$precC, inDF$precM))
    colnames(plotDF2) <- plotDF2[1,]
    plotDF2 <- plotDF2[-1,]
    rownames(plotDF2) <- c("C", "M")
    
    # bar plot
    barplot(plotDF2, horiz=T, col=col.list2,las=2, xlim = c(0,0.8),
            main = "Precipitation predictability")
    
    legend("bottomright", c("C", "M"), bty="n", fill=col.list2)
    
    # precipitation labels
    text(0.45, 24.8, round(inDF[21, "prec_sum"],0))
    text(0.45, 23.6, round(inDF[20, "prec_sum"],0))
    text(0.54, 22.4, round(inDF[19, "prec_sum"],0))
    text(0.54, 21.2, round(inDF[18, "prec_sum"],0))
    text(0.54, 20.0, round(inDF[17, "prec_sum"],0))
    text(0.54, 18.7, round(inDF[16, "prec_sum"],0))
    text(0.5, 17.6, round(inDF[15, "prec_sum"],0))
    text(0.5, 16.4, round(inDF[14, "prec_sum"],0))
    text(0.5, 15.2, round(inDF[13, "prec_sum"],0))
    text(0.5, 14.0, round(inDF[12, "prec_sum"],0))
    text(0.54, 12.7, round(inDF[11, "prec_sum"],0))
    text(0.5, 11.5, round(inDF[10, "prec_sum"],0))
    text(0.6, 10.3, round(inDF[9, "prec_sum"],0))
    text(0.5, 9.0, round(inDF[8, "prec_sum"],0))
    text(0.5, 7.8, round(inDF[7, "prec_sum"],0))
    text(0.5, 6.6, round(inDF[6, "prec_sum"],0))
    text(0.45, 5.4, round(inDF[5, "prec_sum"],0))
    text(0.45, 4.2, round(inDF[4, "prec_sum"],0))
    text(0.4, 3, round(inDF[3, "prec_sum"],0))
    text(0.4, 1.8, round(inDF[2, "prec_sum"],0))
    text(0.4, 0.6, round(inDF[1, "prec_sum"],0))
    
    par(opar)
    
    
}
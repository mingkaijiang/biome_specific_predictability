
####################################################################################
## plot 50th percentile data
plot50th <- function(absDF, pDF, raw) {
    
    ## setting summary dataframe
    abs <- matrix(nrow = 14, ncol = 5)
    abs <- as.data.frame(abs)
    colnames(abs) <- c("biome","temp_mean","prec_mean",
                       "temp_p", "prec_p")
    abs$biome <- c(1:14)
    
    ## Filling biome-specific average values
    for (i in 1:14) {
        abs[abs$biome == i, "temp_mean"] <- mean(raw[raw$BIOME == i, "temp"])
        abs[abs$biome == i, "prec_mean"] <- mean(raw[raw$BIOME == i, "prec_sum"])
        abs[abs$biome == i, "temp_p"] <- mean(raw[raw$BIOME == i, "tempP"])
        abs[abs$biome == i, "prec_p"] <- mean(raw[raw$BIOME == i, "precP"])
    }
    
    # Set output graph structure
    set.panel()
    par(oma=c(2,4,2,2),
        mar=c(5.1,5.1,4.1,1.2),
        mgp = c(3, 1, 0))
    set.panel(5,3)
    
    ## abs climate 50th percentile
    for (i in 1:14) {
        with(absDF[absDF$BIOME == i, ], plot(temp, prec_sum, xlab="Temperature",
                                             ylab="Precipitation", xlim=c(-30,30), main=biome[i],
                                             ylim=c(0, 4000), col=adjustcolor(color.list[i], 0.5),
                                             pch=21))
        with(abs[abs$biome == i, ], points(temp_mean, prec_mean, pch=4, cex=4,
                                           xlim=c(-30,30),ylim=c(0,4000),
                                           col="black"))
    }
    
    plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
    
    ## climatic predictability 50th percentile
    for (i in 1:14) {
        with(absDF[absDF$BIOME == i, ], plot(tempP, precP, xlab="Temperature P",
                                             ylab="Precipitation P", xlim=c(0,1), main=biome[i],
                                             ylim=c(0, 1), col=adjustcolor(color.list[i], 0.5),
                                             pch=21))
        with(abs[abs$biome == i, ], points(temp_p, prec_p, pch=4, cex=4,
                                           xlim=c(0,1),ylim=c(0,1),
                                           col="black"))
    }
    par(opar)
    
} 

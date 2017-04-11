####################################################################################
# Plotting summary profile using 2-dimensional error bars
summary2d <- function(summary) {
    
    # setting graphics
    require(psych)
    m <- matrix(c(1,2,3,3), nrow=2, ncol=2,byrow=T)
    layout(mat=m, heights=c(0.8,0.2))
    
    ## temp vs. prec
    df1 <- data.frame(mean=summary$temp_mean, sd=summary$temp_sd)
    df2 <- data.frame(mean=summary$prec_mean, sd=summary$prec_sd)
    error.crosses(df1, df2, sd=T, color=color.list, xlab="Temperature",
                  ylab="Precipitation", main=NA)
    ## tempP vs. precP
    df1 <- data.frame(mean=summary$tempP_mean, sd=summary$tempP_sd)
    df2 <- data.frame(mean=summary$precP_mean, sd=summary$precP_sd)
    error.crosses(df1, df2, sd=T, color=color.list,
                  xlab = "Temperature P", ylab = "Precipitation P",
                  main=NA)
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
           cex=0.8,ncol=4)
    
    ## temp vs. prec
    with(summary, plot(temp_mean, prec_mean, col=color.list,
                       pch=19, xlim=c(-20,40), ylim=c(0,4000),
                       xlab="Temperature", ylab = "Precipitation"))
    
    ## tempP vs. precP
    with(summary, plot(tempP_mean, precP_mean, col=color.list,
                       pch=19, xlim=c(0,1), ylim=c(0,1),
                       xlab="Temperature P", ylab = "Precipitation P"))
    
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biome, fill=color.list, 
           cex=0.8,ncol=4)
    par(opar)
}

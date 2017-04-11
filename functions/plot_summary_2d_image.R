####################################################################################
# Plotting summary profile using 2-dimensional error bars
summary2dimage <- function(summary) {
    
    # setting graphics
    require(psych)
    m <- matrix(c(1,2,3,3), nrow=2, ncol=2,byrow=T)
    layout(mat=m, heights=c(0.8,0.2))
    
    ## tempP vs. precP
    df1 <- data.frame(mean=summary$tempP_mean, sd=summary$tempP_sd)
    df2 <- data.frame(mean=summary$precP_mean, sd=summary$precP_sd)
    error.crosses(df1, df2, sd=T, color=color.list,
                  xlab = "Temperature P", ylab = "Precipitation P",
                  main=NA, cex.lab = 1.5)
    mtext("(a)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    x.lab <- expression("Temperature [" * degree~C * "]")
    ## temp vs. prec
    df1 <- data.frame(mean=summary$temp_mean, sd=summary$temp_sd)
    df2 <- data.frame(mean=summary$prec_mean, sd=summary$prec_sd)
    error.crosses(df1, df2, sd=T, color=color.list, xlab=x.lab,
                  ylab="Precipitation [mm]", main=NA, cex.lab = 1.5)
    mtext("(b)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    # legend
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
           cex=1,ncol=4)
    
    
    par(opar)
}

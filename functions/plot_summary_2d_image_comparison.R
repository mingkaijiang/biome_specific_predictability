####################################################################################
# Plotting summary profile using 2-dimensional error bars
# compare two time periods
summary_2d_image_comparison <- function(summary1, summary2) {
    
    # setting graphics
    m <- matrix(c(1,2,3,4,5,5), nrow=3, ncol=2, byrow=T)
    layout(mat=m, heights=c(0.4, 0.4, 0.2))
    
    x.lab <- expression("Temperature [" * degree~C * "]")
    
    ## tempP vs. precP 1901-1990
    df1 <- data.frame(mean=summary1$tempP_mean, sd=summary1$tempP_sd)
    df2 <- data.frame(mean=summary1$precP_mean, sd=summary1$precP_sd)
    error.crosses(df1, df2, sd=T, color=color.list,
                  xlab = "Temperature predictability", 
                  ylab = "Precipitation predictability", 
                  main="1901-1990", cex.lab = 1.5, lwd=2, cex=1.5, pch=19,
                  arrow.len=0.03, offset=1.0)
    mtext("(a)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    ## tempP vs. precP 1991-2012
    df1 <- data.frame(mean=summary2$tempP_mean, sd=summary2$tempP_sd)
    df2 <- data.frame(mean=summary2$precP_mean, sd=summary2$precP_sd)
    error.crosses(df1, df2, sd=T, color=color.list,
                  xlab = "Temperature predictability", 
                  ylab = "Precipitation predictability", 
                  main="1991-2012", cex.lab = 1.5, lwd=2, cex=1.5, pch=19,
                  arrow.len=0.03, offset=1.0)
    mtext("(b)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    ## temp vs. prec 1901-1990
    df1 <- data.frame(mean=summary1$temp_mean, sd=summary1$temp_sd)
    df2 <- data.frame(mean=summary1$prec_mean, sd=summary1$prec_sd)
    error.crosses(df1, df2, sd=T, color=color.list, xlab=x.lab,
                  ylab="Precipitation [mm]", main=NA, cex.lab = 1.5,
                  lwd=2, cex=1.5, pch=19,arrow.len=0.03,offset=1.0)
    mtext("(c)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    
    ## temp vs. prec 1991-2012
    df1 <- data.frame(mean=summary2$temp_mean, sd=summary2$temp_sd)
    df2 <- data.frame(mean=summary2$prec_mean, sd=summary2$prec_sd)
    error.crosses(df1, df2, sd=T, color=color.list, xlab=x.lab,
                  ylab="Precipitation [mm]", main=NA, cex.lab = 1.5,
                  lwd=2, cex=1.5, pch=19,arrow.len=0.03,offset=1.0)
    mtext("(d)", side = 3, adj = 0.05, line = -2, cex = 2)
    
    # legend
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
           cex=1.15,ncol=5)
    
    
    par(opar)
}

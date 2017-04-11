####################################################################################
## statistical tests on NDVI and Climate

ndvi_tests <- function(inDF) {
    
    # library
    require(lme4)
    require(gam)
    
    #require(nlme)
    #require(arm)
    
    # read in file
    myDF <- inDF[complete.cases(inDF),]
    
    # setting graphics
    layout(matrix(c(1,2,3,4),2,2))
    
    # Plot NDVI_mean at CRU grids
    with(myDF, quilt.plot(lon, lat, ndviMean,
                          nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # Plot NDVI_P at CRU grids
    with(myDF, quilt.plot(lon, lat, ndviP,
                          nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # Plot NDVI_C at CRU grids
    with(myDF, quilt.plot(lon, lat, ndviC,
                          nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # Plot NDVI_M at CRU grids
    with(myDF, quilt.plot(lon, lat, ndviM,
                          nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    
    # generalized linear mixed model
    #mod1 <- lme(ndviMean~tempP+temp+precP+prec_sum, data=myDF, random=~1|BIOME, method="REML")
    #mod2 <- gls(ndviMean~tempP+temp+precP+prec_sum, data=myDF, method="REML")
    #mod3 <- lme(ndviMean~temp+prec_sum, data=myDF, random=~1|BIOME, method="REML")
    #3anova(mod1,mod2, mod3)
    
    # generalized additive model
    mod1 <- gam(ndviMean~prec_sum, data=myDF)
    mod2 <- update(mod1, ~.+temp)
    mod3 <- update(mod2, ~.+lat)
    mod4 <- update(mod3, ~.+BIOME)
    mod5 <- update(mod4, ~.+precP)
    mod6 <- update(mod5, ~.+tempP)
    mod7 <- update(mod6, ~.+ndviP)
    mod8 <- update(mod7, ~.+ndviC)
    mod9 <- update(mod8, ~.+ndviM)
    mod.sum <- anova(mod1, mod2, mod3, mod4, mod5,
                     mod6, mod7, mod8, mod9, test="Chisq")
    
    # compare models in GAM
    test1 <- predict(mod2)
    test2 <- predict(mod4)
    test3 <- predict(mod6)
    test4 <- predict(mod9)
    
    # Plot key prediction comparisons
    
    # test 1
    plot(myDF$ndviMean, test1, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed NDVI", ylab= "Predicted NDVI")
    abline(lm(test1~myDF$ndviMean), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("NDVI ~ prec + temp")
    
    # test 2
    plot(myDF$ndviMean, test2, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed NDVI", ylab= "Predicted NDVI")
    abline(lm(test2~myDF$ndviMean), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("NDVI ~ prec + temp + lat + biome", cex = 0.5)
    
    # test 3
    plot(myDF$ndviMean, test3, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed NDVI", ylab= "Predicted NDVI")
    abline(lm(test3~myDF$ndviMean), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("NDVI ~ prec + temp + lat + biome 
          + prec P + temp P", cex = 0.5)
    
    # test 4
    plot(myDF$ndviMean, test4, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed NDVI", ylab= "Predicted NDVI")
    abline(lm(test4~myDF$ndviMean), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("NDVI ~ prec + temp + lat + biome \n + prec P + temp P + NDVI P
          + NDVI C + NDVI M", cex = 0.5)
    
    # linear model
    colnames(myDF) <- c("CRU_Site", "lon", "lat", "TP", "TC", "TM",
                        "bio", "Real", "PP", "PC", "PM", "Temp", "Prec", 
                        "Pmea", "temp_group", "prec_group", "templab",
                        "preclab", "tempMC", "tempIE", "precMC", "precIE",
                        "NP", "NC", "NM", "Nmea")
    
    fit <- lm(Nmea~Prec+Temp+PP+TP+lat+bio+NP, data=myDF)
    summary(fit)
    anova(fit)
    #layout(matrix(c(1,2,3,4),2,2))
    plot(fit)
    
    # compare linear model with GAM
    # anova(fit, mod8)
    
    # Calculate Relative Importance for Each Predictor
    require(relaimpo)
    calc.relimp(fit,type=c("lmg","last","first","pratt"),
                rela=TRUE)
    
    # Bootstrap Measures of Relative Importance (1000 samples) 
    boot <- boot.relimp(fit, b = 1000, type = c("lmg", 
                                                "last", "first", "pratt"), rank = TRUE, 
                        diff = TRUE, rela = TRUE)
    booteval.relimp(boot) # print result
    plot(booteval.relimp(boot,sort=TRUE)) # plot result
    
}

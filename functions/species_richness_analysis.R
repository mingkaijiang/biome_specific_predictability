####################################################################################
## Correlate species richness with CRU climates
species_analysis <- function(species, cru, ndvi) {
    
    # library
    require(lme4)
    require(gam)
    
    # binding two data sets
    cru <- cru[order(cru$CRU_Site),]
    species <- species[order(species$CRU_Site),]
    ndvi <- ndvi[order(ndvi$CRU_Site),]
    newDF <- data.frame(cru, species, ndvi)
    
    # plotting species distribution map
    with(newDF, quilt.plot(lon, lat, B_all,
                           nx=300, ny=260, nlevel=12,
                           main="Bird richness"))
    
    # simplifying dataset
    myDF <- newDF[complete.cases(newDF$B_all),]
    myDF <- myDF[complete.cases(myDF$ndviMean),]
    myDF <- myDF[complete.cases(myDF$ndviP),]
    
    # setting graphics
    layout(matrix(c(1,2,3,4),2,2))
    
    # generalized additive model
    mod1 <- gam(B_all~prec_sum, data=myDF)
    mod2 <- update(mod1, ~.+temp)
    mod3 <- update(mod2, ~.+precP)
    mod4 <- update(mod3, ~.+tempP)
    mod5 <- update(mod4, ~.+tempIE)
    mod6 <- update(mod5, ~.+precIE)
    mod7 <- update(mod6, ~.+ndviMean)
    mod8 <- update(mod7, ~.+ndviP)
    mod.sum <- anova(mod1, mod2, mod3, mod4, mod5,
                     mod6, mod7, mod8, test="Chisq")
    
    # compare models in GAM
    test1 <- predict(mod2)
    test2 <- predict(mod4)
    test3 <- predict(mod6)
    test4 <- predict(mod8)
    
    # Plot key prediction comparisons
    
    # test 1
    plot(myDF$B_all, test1, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed", ylab= "Predicted")
    abline(lm(test1~myDF$B_all), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("Bird ~ prec + temp")
    
    # test 2
    plot(myDF$B_all, test2, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed", ylab= "Predicted")
    abline(lm(test2~myDF$B_all), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("Bird ~ prec + temp + precP + tempP", cex = 0.5)
    
    # test 3
    plot(myDF$B_all, test3, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed", ylab= "Predicted")
    abline(lm(test3~myDF$B_all), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("Bird ~ prec + temp + precP + tempP
          + prec IE + temp IE", cex = 0.5)
    
    # test 4
    plot(myDF$B_all, test4, col=adjustcolor("lightgreen", 0.5),
         cex = 0.2, xlab = "Observed", ylab= "Predicted")
    abline(lm(test4~myDF$B_all), col="darkgreen")
    abline(0,1, col="red", lty=2)
    legend("topleft", c("fitted line", "1:1 line"),
           col=c("darkgreen", "red"), lty = c(1, 2))
    title("Bird ~ prec + temp + precP + tempP
          + prec IE + temp IE + NDVI + NDVI P", cex = 0.5)
    
    # linear model
    subDF <- data.frame(myDF$B_all, myDF$tempP, myDF$precP, myDF$temp,
                        myDF$prec_sum, myDF$tempIE, myDF$precIE, 
                        myDF$ndviMean, myDF$ndviP)
    colnames(subDF) <- c("Bird", "TP", "PP", "Temp",
                         "Prec", "TIE", "PIE", "Nmea", "NP")
    
    fit <- lm(Bird~Prec+Temp+PP+TP+TIE+PIE+NP+Nmea, data=subDF)
    summary(fit)
    anova(fit)
    plot(fit)
    
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
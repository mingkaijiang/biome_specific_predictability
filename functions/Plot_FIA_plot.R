####################################################################################
## plot climate impacts on fia statistics
fia_plot <- function(inDF) {
    
    # library
    require(lme4)
    require(gam)
    
    set.panel(3,1)
    
    with(inDF, quilt.plot(LON, LAT, exp, xlim=c(-130,-50),
                          ylim=c(20,50), main = "Exponent",
                          nx=300, ny=140, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    with(inDF, quilt.plot(LON, LAT, int,xlim=c(-130,-50),
                          ylim=c(20,50), main = "Intercept",
                          nx=300, ny=140, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    with(inDF, quilt.plot(LON, LAT, rsq,xlim=c(-130,-50),
                          ylim=c(20,50), main = "R square",
                          nx=300, ny=140, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    set.panel()
    
    # simplifying dataset
    myDF <- inDF[complete.cases(inDF$exp),]
    myDF <- myDF[complete.cases(myDF$temp),]
    
    # setting graphics
    layout(matrix(c(1,2,3,4),2,2))
    
    # generalized additive model
    mod1 <- gam(exp~prec_sum, data=myDF)
    mod2 <- update(mod1, ~.+temp)
    mod3 <- update(mod2, ~.+precP)
    mod4 <- update(mod3, ~.+tempP)
    mod5 <- update(mod4, ~.+tempC)
    mod6 <- update(mod5, ~.+precC)
    mod7 <- update(mod6, ~.+tempM)
    mod8 <- update(mod7, ~.+precM)
    mod.sum <- anova(mod1, mod2, mod3, mod4, mod5,
                     mod6, mod7, mod8, test="Chisq")
    
    # compare models in GAM
    test1 <- predict(mod2)
    test2 <- predict(mod4)
    test3 <- predict(mod6)
    test4 <- predict(mod8)
    
    # Plot key prediction comparisons
    # ignored... ...
    
    # linear model
    subDF <- data.frame(myDF$exp, myDF$tempP, myDF$precP, myDF$temp,
                        myDF$prec_sum, myDF$PC1, myDF$PC2)
    colnames(subDF) <- c("Exp", "TP", "PP", "Temp",
                         "Prec", "PC1", "PC2")
    
    fit <- lm(Exp~Prec+Temp+PP+TP, data=subDF)
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
    
    # linear model with PC only
    fit2 <- lm(Exp~PC1+PC2, data=subDF)
    summary(fit2)
    anova(fit2)
    plot(fit2)
    
    # Calculate Relative Importance for Each Predictor
    calc.relimp(fit2,type=c("lmg","last","first","pratt"),
                rela=TRUE)
    
    # Bootstrap Measures of Relative Importance (1000 samples) 
    boot <- boot.relimp(fit2, b = 1000, type = c("lmg", 
                                                 "last", "first", "pratt"), rank = TRUE, 
                        diff = TRUE, rela = TRUE)
    booteval.relimp(boot) # print result
    plot(booteval.relimp(boot)) # plot result
    
}

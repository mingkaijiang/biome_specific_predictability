####################################################################################
##PCA kernel density analysis using summary statistics
###two period comparison
SummaryPCA_image_comparison <- function(inDF1, inDF2) {
    
    
    # setting graphics
    par(mfrow=c(1,2))

    # process infile
    subDF1 <- data.frame(inDF1$biome, inDF1$temp_mean, inDF1$tempP_mean, 
                        inDF1$prec_mean, inDF1$precP_mean)
    colnames(subDF1) <- c("biome", "temp", "tempP", "prec", "precP")
    subDF1[,1] <- biome
    rownames(subDF1) <- subDF1[,1]
    subDF1$biome <- NULL
    
    # compute PCA statistics
    prin <- princomp(subDF1, cor=T)
    sumtab <- summary(prin)
    PoV <- sumtab$sdev^2/sum(sumtab$sdev^2)
    #loadings(prin)
    
    out <- matrix(ncol=4,nrow=4)
    out <- as.data.frame(out)
    colnames(out) <- c("PC1", "PC2", "PC3", "PC4")
    rownames(out) <- c("temp", "tempP", "prec", "precP")
    
    for(i in 1:4) {
        pc <- as.data.frame(prin$scores[,i])
        out[1,i] <- cor(pc[,1], subDF1$temp)
        out[2,i] <- cor(pc[,1], subDF1$tempP)
        out[3,i] <- cor(pc[,1], subDF1$prec)
        out[4,i] <- cor(pc[,1], subDF1$precP)
    }
    
    # plotting
    #plot(prin, main = "Importance of components")
    
    # Assign principal component values onto x and y axises
    x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
    y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
    
    # plotting
    biplot(prin, xlab = x.lab, ylab= y.lab,
           col=c("red","blue"), cex = c(0.7,0.7),
           main = "1901-1990")
    
    # process infile
    subDF2 <- data.frame(inDF2$biome, inDF2$temp_mean, inDF2$tempP_mean, 
                         inDF2$prec_mean, inDF2$precP_mean)
    colnames(subDF2) <- c("biome", "temp", "tempP", "prec", "precP")
    subDF2[,1] <- biome
    rownames(subDF2) <- subDF2[,1]
    subDF2$biome <- NULL
    
    # compute PCA statistics
    prin <- princomp(subDF2, cor=T)
    sumtab <- summary(prin)
    PoV <- sumtab$sdev^2/sum(sumtab$sdev^2)
    #loadings(prin)
    
    out <- matrix(ncol=4,nrow=4)
    out <- as.data.frame(out)
    colnames(out) <- c("PC1", "PC2", "PC3", "PC4")
    rownames(out) <- c("temp", "tempP", "prec", "precP")
    
    for(i in 1:4) {
        pc <- as.data.frame(prin$scores[,i])
        out[1,i] <- cor(pc[,1], subDF2$temp)
        out[2,i] <- cor(pc[,1], subDF2$tempP)
        out[3,i] <- cor(pc[,1], subDF2$prec)
        out[4,i] <- cor(pc[,1], subDF2$precP)
    }
    
    # plotting
    #plot(prin, main = "Importance of components")
    
    # Assign principal component values onto x and y axises
    x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
    y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
    
    # plotting
    biplot(prin, xlab = x.lab, ylab= y.lab,
           col=c("red","blue"), cex = c(0.7,0.7),
           main="1991-2012")
}
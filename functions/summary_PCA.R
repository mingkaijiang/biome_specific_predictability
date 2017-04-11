
####################################################################################
##PCA kernel density analysis using summary statistics

SummaryPCA <- function(inDF) {
    
    # library
    #require(ks)
    #require(vegan)
    
    # process infile
    subDF <- data.frame(inDF$biome, inDF$temp_mean, inDF$tempP_mean, 
                        inDF$prec_mean, inDF$precP_mean)
    colnames(subDF) <- c("biome", "temp", "tempP", "prec", "precP")
    subDF[,1] <- biome
    rownames(subDF) <- subDF[,1]
    subDF$biome <- NULL
    
    # compute PCA statistics
    prin <- princomp(subDF, cor=T)
    sumtab <- summary(prin)
    PoV <- sumtab$sdev^2/sum(sumtab$sdev^2)
    #loadings(prin)
    
    out <- matrix(ncol=4,nrow=4)
    out <- as.data.frame(out)
    colnames(out) <- c("PC1", "PC2", "PC3", "PC4")
    rownames(out) <- c("temp", "tempP", "prec", "precP")
    
    for(i in 1:4) {
        pc <- as.data.frame(prin$scores[,i])
        out[1,i] <- cor(pc[,1], subDF$temp)
        out[2,i] <- cor(pc[,1], subDF$tempP)
        out[3,i] <- cor(pc[,1], subDF$prec)
        out[4,i] <- cor(pc[,1], subDF$precP)
    }
    
    # plotting
    plot(prin, main = "Importance of components")
    
    # Assign principal component values onto x and y axises
    x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
    y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
    
    # plotting
    biplot(prin, xlab = x.lab, ylab= y.lab,
           col=c("red","blue"))
}

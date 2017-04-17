####################################################################################
##PCA kernel density analysis using all data
BiomePCA <- function(inDF) {
    
    # library
    #require(vegan)
    #require(ks)
    
    # read in file
    myDF <- inDF
    
    # generate outDF to save coefficient coefficients for all biomes
    outDF <- as.data.frame(matrix(ncol=28, nrow=5))
    colnames(outDF) <- c("B1P1","B1P2","B2P1","B2P2","B3P1","B3P2",
                         "B4P1","B4P2","B5P1","B5P2","B6P1","B6P2",
                         "B7P1","B7P2","B8P1","B8P2","B9P1","B9P2",
                         "B10P1","B10P2","B11P1","B11P2","B12P1","B12P2",
                         "B13P1","B13P2","B14P1","B14P2")
    rownames(outDF) <- c("temp", "tempP", "prec", "precP","VarianceExplained")
    
    
    for (i in 1:14) {
        biomeDF <- subset(myDF, BIOME==i)
        newDF <- data.frame(biomeDF$temp,biomeDF$tempP,
                            biomeDF$prec_sum,biomeDF$precP)
        names(newDF) <- c("temp","tempP", "prec", "precP")
        
        #################### PCA statistics #####################################
        # select the 4 climate variables
        dat2<-newDF
        
        # use princomp for PCA for being consistent with scaling of scores in Sandra's analysis 
        prin<-princomp((dat2), cor = TRUE, scores = TRUE)
        pc12<-prin$scores[,1:2]
        ll<-prin$loadings
        sumtab <- summary(prin)
        PoV <- sumtab$sdev^2/sum(sumtab$sdev^2)
        
        # generate out table to save correlation coefficients
        out <- matrix(ncol=2,nrow=4)
        out <- as.data.frame(out)
        colnames(out) <- c("PC1", "PC2")
        rownames(out) <- c("temp", "tempP", "prec", "precP")
        
        for(j in 1:2) {
            pc <- prin$scores[,j]
            out[1,j] <- cor(pc, dat2$temp)
            out[2,j] <- cor(pc, dat2$tempP)
            out[3,j] <- cor(pc, dat2$prec)
            out[4,j] <- cor(pc, dat2$precP)
        }
        
        n <- i - 1
        s <- 2 * n + 1
        e <- s+1
        outDF[1:4,s:e] <- out[1:4,1:2]
        outDF[5,s] <- round(PoV[[1]]*100,0)
        outDF[5,e] <- round(PoV[[2]]*100,0)
        
        # Assign principal component values onto x and y axises
        x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
        y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
        
        l <- nrow(pc12)
        
        if(l <= 1000) {
            H <- Hpi(x=pc12)      # optimal bandwidth estimation
            est<- kde(x=pc12, H=H, compute.cont=TRUE)     # kernel density estimation
        } else {
            pc12.sub <- pc12[sample(nrow(pc12), 1000),]
            # kernel density estimation
            H <- Hpi(x=pc12.sub)      # optimal bandwidth estimation
            est<- kde(x=pc12.sub, H=H, compute.cont=TRUE)     # kernel density estimation
        }
        
        # set contour probabilities for drawing contour levels
        cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
        
        fit<-envfit(pc12, dat2) # use envfit for drawing arrows, can be also done using trait loadings
        fit2<-fit$vectors$arrows*-1 # drawing line segments in arrow opposites direction for pretty layout
        
        plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, ylab=y.lab, xlab=x.lab, 
             cex.axis=0.75, ylim=c(-10, 10), xlim=c(-10, 10),las=1) 
        plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
        plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        #points( pc12[,], pch=16, cex=0.25, col="black") 
        plot(fit, cex=0.90, col=1, labels=list(vectors = c("temp", "prec", "temp P", "prec P")))
        #segments(0,0, fit2[,1], fit2[,2], col=1, lty=2, lwd=1)
        #mtext("PC1", cex=0.75, side=1, line=0.5, adj=1)
        #mtext("PC2", cex=0.75, side=2, line=0.5, adj=1)
        title(paste(biome[i]))
    }
    
    write.table(outDF, paste0(dataDir, "/biome_specific_PCA_statistics.csv"),
                col.names=T, row.names=F, sep=",")
}
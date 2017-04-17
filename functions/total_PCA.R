####################################################################################
##PCA kernel density analysis using all data, regardless of biome
TotalPCA <- function(inDF) {
    
    # library
    #require(vegan)
    #require(ks)
    
    # read in file
    myDF <- inDF
    
    # select the 4 climate variables
    dat2 <- data.frame(myDF$temp,myDF$tempP,
                       myDF$prec_sum,myDF$precP)
    names(dat2) <- c("temp","tempP", "prec", "precP")
    
    #################### PCA statistics ####################################
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
    
    myDF$PC1 <- pc12[,1]
    myDF$PC2 <- pc12[,2]
    
    # Assign principal component values onto x and y axises
    x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
    y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
    
    for (k in c(100, 200, 500, 1000, 5000)) {
        pc12.sub <- pc12[sample(nrow(pc12),k), ]  # to speed up the process
        
        ################ KERNEL DENSITY ESTIMATION ##############################
        H <- Hpi(x=pc12.sub)      # optimal bandwidth estimation
        est<- kde(x=pc12.sub, H=H, compute.cont=TRUE)     # kernel density estimation
        
        # set contour probabilities for drawing contour levels
        cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
        
        fit<-envfit(pc12, dat2) 
        fit2<-fit$vectors$arrows*-1 
        
        plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, ylab=y.lab, xlab=x.lab, 
             cex.axis=0.75, ylim=c(-10, 10), xlim=c(-10, 10),las=1) 
        plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
        plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
        plot(fit, cex=0.90, col=1, labels=list(vectors = c("temp", "prec", "temp P", "prec P")))
        legend("topright", paste0("n = ", k))
    
    }
    
    write.table(myDF, paste(dataDir, "/CRU_with_PCA.csv", sep=""),
                col.names=T,row.names=F,sep=",")
}
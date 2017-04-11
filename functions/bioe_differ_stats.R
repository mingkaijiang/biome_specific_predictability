####################################################################################
BiomeDifferStats <- function(inDF) {
    # Check PCM for temp and prec where MAT and MAP overlaps for 14 biomes
    
    # create output matrix to save conditional groups
    mmP <- matrix(ncol=14,nrow=14)    # predcitability
    mmC <- matrix(ncol=14,nrow=14)    # constancy
    mmM <- matrix(ncol=14,nrow=14)    # contingency
    
    
    ####################################################################################
    # sub function in BiomeDifferStats
    ConCheckP <- function()
    {
        # ckecking number of data points
        l1 <- length(newDF[newDF$BIOME == i, "tempP"])
        l2 <- length(newDF[newDF$BIOME == j, "tempP"])
        
        # statistical tests
        ifelse(l1 >= 5 & l2 >= 5,
               p1 <- t.test(newDF[newDF$BIOME==i, "tempP"], newDF[newDF$BIOME == j, "tempP"],
                            alternative="two.sided")$p.value, 
               p1 <- 1)
        
        ifelse(l1 >= 5 & l2 >= 5,
               p2 <- t.test(newDF[newDF$BIOME==i, "precP"], newDF[newDF$BIOME == j, "precP"],
                            alternative="two.sided")$p.value, 
               p2 <- 1)
        
        out <- c(p1,p2)
        
        return(out)
    }
    ####################################################################################
    # sub function in BiomeDifferStats
    ConCheckC <- function()
    {
        # ckecking number of data points
        l1 <- length(newDF[newDF$BIOME == i, "tempC"])
        l2 <- length(newDF[newDF$BIOME == j, "tempC"])
        
        # statistical tests
        ifelse(l1 >= 5 & l2 >= 5,
               p1 <- t.test(newDF[newDF$BIOME==i, "tempC"], newDF[newDF$BIOME == j, "tempC"],
                            alternative="two.sided")$p.value, 
               p1 <- 1)
        
        ifelse(l1 >= 5 & l2 >= 5,
               p2 <- t.test(newDF[newDF$BIOME==i, "precC"], newDF[newDF$BIOME == j, "precC"],
                            alternative="two.sided")$p.value, 
               p2 <- 1)
        
        out <- c(p1,p2)
        
        return(out)
    }
    ####################################################################################
    # sub function in BiomeDifferStats
    ConCheckM <- function()
    {
        # ckecking number of data points
        l1 <- length(newDF[newDF$BIOME == i, "tempC"])
        l2 <- length(newDF[newDF$BIOME == j, "tempC"])
        
        # statistical tests
        ifelse(l1 >= 5 & l2 >= 5,
               p1 <- t.test(newDF[newDF$BIOME==i, "tempM"], newDF[newDF$BIOME == j, "tempM"],
                            alternative="two.sided")$p.value, 
               p1 <- 1)
        
        ifelse(l1 >= 5 & l2 >= 5,
               p2 <- t.test(newDF[newDF$BIOME==i, "precM"], newDF[newDF$BIOME == j, "precM"],
                            alternative="two.sided")$p.value, 
               p2 <- 1)
        
        out <- c(p1,p2)
        
        return(out)
    }
    
    for (i in 1:14) {
        for (j in 1:14) {
            
            # subsetting datasets
            max.prec1 <- max(inDF[inDF$BIOME == i,"prec_sum"])
            max.prec2 <- max(inDF[inDF$BIOME == j,"prec_sum"])
            
            min.prec1 <- min(inDF[inDF$BIOME == i,"prec_sum"])
            min.prec2 <- min(inDF[inDF$BIOME == j,"prec_sum"])
            
            max.temp1 <- max(inDF[inDF$BIOME == i,"temp"])
            max.temp2 <- max(inDF[inDF$BIOME == j,"temp"])
            
            min.temp1 <- min(inDF[inDF$BIOME == i,"temp"])
            min.temp2 <- min(inDF[inDF$BIOME == j,"temp"])
            
            max.prec3 <- min(max.prec1, max.prec2)
            min.prec3 <- max(min.prec1, min.prec2)
            
            max.temp3 <- min(max.temp1, max.temp2)
            min.temp3 <- max(min.temp1, min.temp2)
            
            newDF <- subset(inDF, BIOME == i | BIOME == j)
            newDF <- subset(newDF, prec_sum <= max.prec3 & prec_sum >= min.prec3)
            newDF <- subset(newDF, temp <= max.temp3 & temp >= min.temp3)
            
            # ckecking number of data points
            l1 <- length(newDF[newDF$BIOME == i, "tempP"])
            l2 <- length(newDF[newDF$BIOME == j, "tempP"])
            
            # Fill mmP
            ifelse(i == j, mmP[i,j] <- 0,   # same biome, no color
                   ifelse(l1 <= 5 & l2 <= 5, mmP[i,j]<-1,  # MAT and MAP not overlapping
                          ifelse(ConCheckP()[1] <= 0.05 & ConCheckP()[2] <= 0.05, mmP[i,j]<-4, # both significant different
                                 ifelse(ConCheckP()[1]<= 0.05, mmP[i,j]<-2,   # temp significant different
                                        ifelse(ConCheckP()[2] <= 0.05, mmP[i,j]<-3, # prec significant different
                                               mmP[i,j]<-5)))))  # temp and prec P same
            
            # Fill mmC
            ifelse(i == j, mmC[i,j] <- 0,   # same biome, no color
                   ifelse(l1 <= 5 & l2 <= 5, mmC[i,j]<-1,  # MAT and MAP not overlapping
                          ifelse(ConCheckC()[1] <= 0.05 & ConCheckC()[2] <= 0.05, mmC[i,j]<-4, # both significant different
                                 ifelse(ConCheckC()[1]<= 0.05, mmC[i,j]<-2,   # temp significant different
                                        ifelse(ConCheckC()[2] <= 0.05, mmC[i,j]<-3, # prec significant different
                                               mmC[i,j]<-5)))))  # temp and prec C same
            
            # Fill mmM
            ifelse(i == j, mmM[i,j] <- 0,   # same biome, no color
                   ifelse(l1 <= 5 & l2 <= 5, mmM[i,j]<-1,  # MAT and MAP not overlapping
                          ifelse(ConCheckM()[1] <= 0.05 & ConCheckM()[2] <= 0.05, mmM[i,j]<-4, # both significant different
                                 ifelse(ConCheckM()[1]<= 0.05, mmM[i,j]<-2,   # temp significant different
                                        ifelse(ConCheckM()[2] <= 0.05, mmM[i,j]<-3, # prec significant different
                                               mmM[i,j]<-5)))))  # temp and prec M same
            
        }  # j loop
    }   # i loop
    
    #outDF <- cbind(mmP, mmC, mmM)
    #return(outDF)
    
    # Plottings
    plotMatrixCol(mmP, "Predictability")
    
    plotMatrixCol(mmC,"Constancy")
    
    plotMatrixCol(mmM,"Contingency")
    
}  #function loop

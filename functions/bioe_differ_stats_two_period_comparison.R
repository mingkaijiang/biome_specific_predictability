####################################################################################
BiomeDifferStats_comparison <- function(inDF1, inDF2) {
    # Check PCM for temp and prec where MAT and MAP overlaps for 14 biomes
    
    # create output matrix to save conditional groups
    mmP1 <- matrix(ncol=14,nrow=14)    # predcitability
    mmP2 <- matrix(ncol=14,nrow=14)    # predcitability
    
    
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
    for (i in 1:14) {
        for (j in 1:14) {
            
            # subsetting datasets
            max.prec1 <- max(inDF1[inDF1$BIOME == i,"prec_sum"])
            max.prec2 <- max(inDF1[inDF1$BIOME == j,"prec_sum"])
            
            min.prec1 <- min(inDF1[inDF1$BIOME == i,"prec_sum"])
            min.prec2 <- min(inDF1[inDF1$BIOME == j,"prec_sum"])
            
            max.temp1 <- max(inDF1[inDF1$BIOME == i,"temp"])
            max.temp2 <- max(inDF1[inDF1$BIOME == j,"temp"])
            
            min.temp1 <- min(inDF1[inDF1$BIOME == i,"temp"])
            min.temp2 <- min(inDF1[inDF1$BIOME == j,"temp"])
            
            max.prec3 <- min(max.prec1, max.prec2)
            min.prec3 <- max(min.prec1, min.prec2)
            
            max.temp3 <- min(max.temp1, max.temp2)
            min.temp3 <- max(min.temp1, min.temp2)
            
            newDF <- subset(inDF1, BIOME == i | BIOME == j)
            newDF <- subset(newDF, prec_sum <= max.prec3 & prec_sum >= min.prec3)
            newDF <- subset(newDF, temp <= max.temp3 & temp >= min.temp3)
            
            # ckecking number of data points
            l1 <- length(newDF[newDF$BIOME == i, "tempP"])
            l2 <- length(newDF[newDF$BIOME == j, "tempP"])
            
            # Fill mmP1
            ifelse(i == j, mmP1[i,j] <- 0,   # same biome, no color
                   ifelse(l1 <= 5 & l2 <= 5, mmP1[i,j]<-1,  # MAT and MAP not overlapping
                          ifelse(ConCheckP()[1] <= 0.05 & ConCheckP()[2] <= 0.05, mmP1[i,j]<-4, # both significant different
                                 ifelse(ConCheckP()[1]<= 0.05, mmP1[i,j]<-2,   # temp significant different
                                        ifelse(ConCheckP()[2] <= 0.05, mmP1[i,j]<-3, # prec significant different
                                               mmP1[i,j]<-5)))))  # temp and prec P same
            
        }  # j loop
    }   # i loop
    
    ####################################################################################
    for (i in 1:14) {
        for (j in 1:14) {
            
            # subsetting datasets
            max.prec1 <- max(inDF2[inDF2$BIOME == i,"prec_sum"])
            max.prec2 <- max(inDF2[inDF2$BIOME == j,"prec_sum"])
            
            min.prec1 <- min(inDF2[inDF2$BIOME == i,"prec_sum"])
            min.prec2 <- min(inDF2[inDF2$BIOME == j,"prec_sum"])
            
            max.temp1 <- max(inDF2[inDF2$BIOME == i,"temp"])
            max.temp2 <- max(inDF2[inDF2$BIOME == j,"temp"])
            
            min.temp1 <- min(inDF2[inDF2$BIOME == i,"temp"])
            min.temp2 <- min(inDF2[inDF2$BIOME == j,"temp"])
            
            max.prec3 <- min(max.prec1, max.prec2)
            min.prec3 <- max(min.prec1, min.prec2)
            
            max.temp3 <- min(max.temp1, max.temp2)
            min.temp3 <- max(min.temp1, min.temp2)
            
            newDF <- subset(inDF2, BIOME == i | BIOME == j)
            newDF <- subset(newDF, prec_sum <= max.prec3 & prec_sum >= min.prec3)
            newDF <- subset(newDF, temp <= max.temp3 & temp >= min.temp3)
            
            # ckecking number of data points
            l1 <- length(newDF[newDF$BIOME == i, "tempP"])
            l2 <- length(newDF[newDF$BIOME == j, "tempP"])
            
            # Fill mmP2
            ifelse(i == j, mmP2[i,j] <- 0,   # same biome, no color
                   ifelse(l1 <= 5 & l2 <= 5, mmP2[i,j]<-1,  # MAT and MAP not overlapping
                          ifelse(ConCheckP()[1] <= 0.05 & ConCheckP()[2] <= 0.05, mmP2[i,j]<-4, # both significant different
                                 ifelse(ConCheckP()[1]<= 0.05, mmP2[i,j]<-2,   # temp significant different
                                        ifelse(ConCheckP()[2] <= 0.05, mmP2[i,j]<-3, # prec significant different
                                               mmP2[i,j]<-5)))))  # temp and prec P same
            
        }  # j loop
    }   # i loop
    
    mmP1[upper.tri(mmP1)] <- NA
    mmP2[upper.tri(mmP2)] <- NA
    
    
    # Plottings
    plotMatrixCol_comparison(mmP1, mmP2, "Predictability")
    
    
}  #function loop

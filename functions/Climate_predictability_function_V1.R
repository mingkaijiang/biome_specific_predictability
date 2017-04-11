
####################################################################################
# Plot biome map
biomePlot <- function(inDF) {
  
  # setting graphics
  m <- matrix(c(1,2), nrow=2, ncol=1,byrow=T)
  layout(mat=m, heights=c(0.8,0.2))
  
  # plotting
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME > 0, ], 
       quilt.plot(lon, lat, BIOME,
                  nx=300, ny=260, col=color.list,
                  add.legend=F))
  world(add=T)
  
  # legend
  par(mar = rep(2, 4))
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
         cex=0.8,ncol=4)

  par(opar)
}


####################################################################################
BiomeDifferPlot <- function(inDF) {
  # Check if P helps to differ biomes with similar Temp and Prec profiles
  
  for (i in 1:14) {
    for (j in 1:14) {
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
      
      if (dim(newDF)[1] > 0) {plotBoxplot(newDF)}
      
    }
  }
}

plotBoxplot <- function(newDF) {
  # graphics
  m <- matrix(c(1,4,2,5,3,6), nrow=3, ncol=2,byrow=T)
  layout(mat=m, heights=c(0.33, 0.33, 0.33))
  
  # plotting
  with(newDF, boxplot(tempP~BIOME, ylab = "Temperature predictability",
                      main = "Temperature"))
  with(newDF, boxplot(tempC~BIOME, ylab = "Temperature constancy"))
  with(newDF, boxplot(tempM~BIOME, ylab = "Temperature contingency"))
  
  with(newDF, boxplot(precP~BIOME, ylab = "Precipitation predictability",
                      main = "Precipitation"))
  with(newDF, boxplot(precC~BIOME, ylab = "Precipitation constancy"))
  with(newDF, boxplot(precM~BIOME, ylab = "Precipitation contingency"))
}

####################################################################################
plot3d_MAT <- function(myDF) {
  # plot 3d with MAT, C and M for temp
  
  ## Explore 3-d plottings
  require(plot3D)
  
  ##Plot each biome separately
  with(myDF[myDF$BIOME == 1, ], scatter3D(tempC, tempM, temp,
                                          xlim=c(0,1), ylim=c(0,1),
                                          zlim=c(-30,50), xlab="Constancy",
                                          ylab = "Contingency",
                                          zlab = "MAT",
                                          pch = 19, col=color.list[1]))
  for (i in 2: 14) {
    with(myDF[myDF$BIOME == i, ], points3D(tempC, tempM, temp,
                                           pch = 19, col=color.list[i], add=T))
  }
  
}
####################################################################################
plot3d_MAP <- function(myDF) {
  # plot 3d with MAP, C and M for prec
  
  ## Explore 3-d plottings
  require(plot3D)
  
  ##Plot each biome separately
  with(myDF[myDF$BIOME == 1, ], scatter3D(precC, precM, prec_sum,
                                          xlim=c(0,1), ylim=c(0,1),
                                          zlim=c(0,8000), xlab="Constancy",
                                          ylab = "Contingency",
                                          zlab = "MAP",
                                          pch = 19, col=color.list[1]))
  for (i in 2: 14) {
    with(myDF[myDF$BIOME == i, ], points3D(precC, precM, prec_sum,
                                           pch = 19, col=color.list[i], add=T))
  }
  
}
####################################################################################
aridity <- function(inDF) {
  library(rgdal)
  library(RColorBrewer)
  dpath<- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/aridity/Al_annual/ai_yr/w001001.adf"
  
  x <- new("GDALReadOnlyDataset", dpath)
  getDriver(x)
  getDriverLongName(getDriver(x))
  xx<-asSGDF_GROD(x)
  r <- raster(xx)
  
}

####################################################################################
####################################################################################
# sub function in BiomeDifferStats
plotMatrixCol <- function(inDF, title.name) {
  
  # library
  library(gplots)
  
  # need data as matrix
  #mm <- matrix(rexp(196), 14)
  mm <- inDF
  
  heatmap.2(x = mm, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
            col=c("white","lightgrey","red","blue","yellow","green"),
            colsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
            rowsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
            sepcolor="black",sepwidth=c(0.02,0.02),
            trace = "none", key = F,key.title = title.name,
            margins = c(7, 11))
  legend(0,1, legend=c("no overlap", "temp P", "prec P", "both", "neither"),
         fill=c("lightgrey","red","blue","yellow","green"),cex=.8)
  
}

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


####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################

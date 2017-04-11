####################################################################################
##Calculate climate predictability
##Function version 1
##Created by MJ 
##Last modified on 11-06-2016
####################################################################################

library(raster)
library(chron)
library(RColorBrewer)
library(lattice)
library(plyr)
library(data.table)
library(reshape2)
library(maps)
library(changepoint)
#library(SDMTools)
#library(sm)
library(fields)
#library(spatstat)
library(vioplot)
#require(aplpack)
require(plyr)
require(ggplot2)

####################################################################################
## Convert nc file into csv file for CRU temperature and precipitation data
ncTOcsv <- function(inFile, outFile) {
  
  # library 
  library(ncdf4)
  
  # process file
  ncDF <- nc_open(inFile) 
  
  print(ncDF)
  
  t <- ncvar_get(ncDF,varid = "time" )
  nt <- dim(t)
  
  tmp.array <- ncvar_get(ncDF)
  dim(tmp.array)
  
  lon <- ncvar_get(ncDF, "lon")
  nlon <- dim(lon)
  head(lon)
  
  lat <- ncvar_get(ncDF, "lat")
  nlat <- dim(lat)
  head(lat)
  
  jan.list <- 13
  feb.list <- 14
  mar.list <- 15
  apr.list <- 16
  may.list <- 17
  jun.list <- 18
  jul.list <- 19
  aug.list <- 20
  sep.list <- 21
  oct.list <- 22
  nov.list <- 23
  dec.list <- 24
  
  for (i in 2:((nt/12)-1))
  {
    jan.list <- append(jan.list, (1+(i*12)))
    feb.list <- append(feb.list, (2+(i*12)))
    mar.list <- append(mar.list, (3+(i*12)))
    apr.list <- append(apr.list, (4+(i*12)))
    may.list <- append(may.list, (5+(i*12)))
    jun.list <- append(jun.list, (6+(i*12)))
    jul.list <- append(jul.list, (7+(i*12)))
    aug.list <- append(aug.list, (8+(i*12)))
    sep.list <- append(sep.list, (9+(i*12)))
    oct.list <- append(oct.list, (10+(i*12)))
    nov.list <- append(nov.list, (11+(i*12)))
    dec.list <- append(dec.list, (12+(i*12)))
  }
  
  
  ##Jan list
  i <- 1
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  janDF <- as.data.frame(iDF)
  
  quilt.plot(janDF$x, janDF$y, janDF$value, nrow=360, ncol=720)
  
  janDF[,"sequence"] <- 1
  janDF[,"CRU_Site"] <- c(1:length(janDF$x))
  
  for (i in jan.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    janDF <- rbind(janDF,mid)
    
  }
  
  
  ##Feb list
  i <- 2
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  febDF <- as.data.frame(iDF)
  
  febDF[,"sequence"] <- 2
  febDF[,"CRU_Site"] <- c(1:length(febDF$x))
  
  for (i in feb.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    febDF <- rbind(febDF,mid)
  }
  
  ##Mar list
  i <- 3
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  marDF <- as.data.frame(iDF)
  
  marDF[,"sequence"] <- 3
  marDF[,"CRU_Site"] <- c(1:length(marDF$x))
  
  for (i in mar.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    marDF <- rbind(marDF,mid)
    
  }
  
  ##Apr list
  i <- 4
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  aprDF <- as.data.frame(iDF)
  
  aprDF[,"sequence"] <- i
  aprDF[,"CRU_Site"] <- c(1:length(aprDF$x))
  
  for (i in apr.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    aprDF <- rbind(aprDF,mid)
    
  }
  
  ##May list
  i <- 5
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  mayDF <- as.data.frame(iDF)
  
  mayDF[,"sequence"] <- i
  mayDF[,"CRU_Site"] <- c(1:length(mayDF$x))
  
  for (i in may.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    mayDF <- rbind(mayDF,mid)
    
  }
  
  ##Jun list
  i <- 6
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  junDF <- as.data.frame(iDF)
  
  junDF[,"sequence"] <- i
  junDF[,"CRU_Site"] <- c(1:length(junDF$x))
  
  for (i in jun.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    junDF <- rbind(junDF,mid)
    
  }
  
  ##Jul list
  i <- 7
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  julDF <- as.data.frame(iDF)
  
  julDF[,"sequence"] <- i
  julDF[,"CRU_Site"] <- c(1:length(julDF$x))
  
  for (i in jul.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    julDF <- rbind(julDF,mid)
    
  }
  
  ##Aug list
  i <- 8
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  augDF <- as.data.frame(iDF)
  
  augDF[,"sequence"] <- i
  augDF[,"CRU_Site"] <- c(1:length(augDF$x))
  
  for (i in aug.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    augDF <- rbind(augDF,mid)
    
  }
  
  ##Sep list
  i <- 9
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  sepDF <- as.data.frame(iDF)
  
  sepDF[,"sequence"] <- i
  sepDF[,"CRU_Site"] <- c(1:length(sepDF$x))
  
  for (i in sep.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    sepDF <- rbind(sepDF,mid)
    
  }
  
  ##Oct list
  i <- 10
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  octDF <- as.data.frame(iDF)
  
  octDF[,"sequence"] <- i
  octDF[,"CRU_Site"] <- c(1:length(octDF$x))
  
  for (i in oct.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    octDF <- rbind(octDF,mid)
    
  }
  
  
  ##Nov list
  i <- 11
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  novDF <- as.data.frame(iDF)
  
  novDF[,"sequence"] <- i
  novDF[,"CRU_Site"] <- c(1:length(novDF$x))
  
  for (i in nov.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    novDF <- rbind(novDF,mid)
    
  }
  
  ##Dec list
  i <- 12
  
  sliceDF <- tmp.array[,,i]
  sliceDF <- t(sliceDF)
  
  iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
            yrow = seq(-89.75,89.75,by=0.5)) 
  
  decDF <- as.data.frame(iDF)
  
  decDF[,"sequence"] <- i
  decDF[,"CRU_Site"] <- c(1:length(decDF$x))
  
  for (i in dec.list)
  {
    sliceDF <- tmp.array[,,i]
    sliceDF <- t(sliceDF)
    
    iDF <- im(sliceDF, xcol = seq(-179.75,179.75, by=0.5), 
              yrow = seq(-89.75,89.75,by=0.5)) 
    
    mid <- as.data.frame(iDF)
    
    mid[,"sequence"] <- i
    mid[,"CRU_Site"] <- c(1:length(mid$x))
    
    decDF <- rbind(decDF,mid)
    
  }
  
  output<- janDF[,1:2]
  output[,"CRU_Site"] <- janDF[,"CRU_Site"]
  output[,"jan"] <- janDF[,"value"]
  output[,"feb"] <- febDF[,"value"]
  output[,"mar"] <- marDF[,"value"]
  output[,"apr"] <- aprDF[,"value"]
  output[,"may"] <- mayDF[,"value"]
  output[,"jun"] <- junDF[,"value"]
  output[,"jul"] <- julDF[,"value"]
  output[,"aug"] <- augDF[,"value"]
  output[,"sep"] <- sepDF[,"value"]
  output[,"oct"] <- octDF[,"value"]
  output[,"nov"] <- novDF[,"value"]
  output[,"dec"] <- decDF[,"value"]
  
  output[,"sequence"] <- janDF[,"sequence"]
  
  jan.list <- append(1,jan.list)
  
  for (i in 1:length(jan.list))
  {
    output[output$sequence == jan.list[i], "sequence"] <- (i+1900)
  }
  
  setnames(output, c("x","y","CRU_Site","jan","feb","mar","apr","may","jun",
                     "jul","aug","sep","oct","nov","dec","sequence"),
           c("lon","lat","CRU_Site","jan","feb","mar","apr","may","jun",
             "jul","aug","sep","oct","nov","dec","year"))
  
  output <- output[c("CRU_Site","lon","lat","year",
                     "jan","feb","mar","apr","may","jun",
                     "jul","aug","sep","oct","nov","dec")]
  
  output <- data.table(output)
  
  
  write.table(output, outFile, 
              row.names = F,col.names=T, sep = ",")
  
  nc_close(ncDF)

}

####################################################################################
## Remove duplicated entries in the raw data
reduplicate <- function(inFile, outFile) {
  input <- read.table(inFile, sep=",",header=T)
  
  for (i in 1:max(input$CRU_Site))
  {
    mid <- subset(input, CRU_Site == i)
    v1 <- length(unique(mid$jan))
    
    if(v1 == 1)
    {
      input <- input[!(input$CRU_Site == i),]
    }
  }
  
  write.table(input,outFile, 
              sep=",", col.names=T, row.names=F)
}

####################################################################################
##Function to calculate Colwell index for temperature
##112 years of monthly data, absolute values, 12 bin sizes
##Classification scheme: -3 stdev + mean to +3 stdev + mean
PCM_temp<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  ##  inName <- paste(sourceDir, "temp_DF_processed.csv",sep="/")  ##removed duplicated temp data
  inName <- paste(sourceDir, "temp_DF.csv",sep="/")
  outName <- paste(destDir, "temp_PCM.csv", sep="/")
  
  input <- read.table(inName, sep=",", header=T)
  
  temp <- subset(input, year == 1901)
  
  dd <- as.data.frame(input[,5:16])
  
  base.value <- round(mean(colMeans(dd)),2)
  sd.value <- round(sd(sapply(dd, sd)),2)
  
  output <- matrix(nrow=nrow(temp),ncol=24)
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("CRU_Site", "lon","lat",
                        "year","year_count","seasons",
                        "HofX","HofY","HofXY","HXofY",
                        "s","t",
                        "P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output[,1:3] <- temp[,1:3]
  
  for (i in 1:nrow(temp))
  {
    
    X <- input[input$CRU_Site == i,]
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years+1
    
    interval <- 12
    
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","jan","feb","mar","apr","may","jun",
                                 "jul","aug","sep","oct","nov","dec","whole"))
    
    bin[,"bin_size"] <- c("-2.5","-2","-1.5","-1","-0.5","0","0.5","1","1.5","2","2.5",">2.5")
    
    breaks = c(base.value-20*sd.value, 
               base.value-2.5*sd.value, base.value-2.0*sd.value, base.value-1.5*sd.value,
               base.value-1.0*sd.value, base.value-0.5*sd.value, base.value,
               base.value+0.5*sd.value, base.value+1.0*sd.value, base.value+1.5*sd.value,
               base.value+2.0*sd.value, base.value+2.5*sd.value, base.value+20*sd.value)
    
    jan_cut = cut(X$jan, breaks, include.lowest=TRUE,right=TRUE)
    feb_cut = cut(X$feb, breaks, include.lowest=TRUE,right=TRUE)
    mar_cut = cut(X$mar, breaks, include.lowest=TRUE,right=TRUE)
    apr_cut = cut(X$apr, breaks, include.lowest=TRUE,right=TRUE)
    may_cut = cut(X$may, breaks, include.lowest=TRUE,right=TRUE)
    jun_cut = cut(X$jun, breaks, include.lowest=TRUE,right=TRUE)
    jul_cut = cut(X$jul, breaks, include.lowest=TRUE,right=TRUE)
    aug_cut = cut(X$aug, breaks, include.lowest=TRUE,right=TRUE)
    sep_cut = cut(X$sep, breaks, include.lowest=TRUE,right=TRUE)
    oct_cut = cut(X$oct, breaks, include.lowest=TRUE,right=TRUE)
    nov_cut = cut(X$nov, breaks, include.lowest=TRUE,right=TRUE)
    dec_cut = cut(X$dec, breaks, include.lowest=TRUE,right=TRUE)
    
    jan_freq = table(jan_cut)
    feb_freq = table(feb_cut)
    mar_freq = table(mar_cut)
    apr_freq = table(apr_cut)
    may_freq = table(may_cut)
    jun_freq = table(jun_cut)
    jul_freq = table(jul_cut)
    aug_freq = table(aug_cut)
    sep_freq = table(sep_cut)
    oct_freq = table(oct_cut)
    nov_freq = table(nov_cut)
    dec_freq = table(dec_cut)
    
    bin[,"jan"] <- jan_freq
    bin[,"feb"] <- feb_freq
    bin[,"mar"] <- mar_freq
    bin[,"apr"] <- apr_freq
    bin[,"may"] <- may_freq
    bin[,"jun"] <- jun_freq
    bin[,"jul"] <- jul_freq
    bin[,"aug"] <- aug_freq
    bin[,"sep"] <- sep_freq
    bin[,"oct"] <- oct_freq
    bin[,"nov"] <- nov_freq
    bin[,"dec"] <- dec_freq
    
    newbin <- as.numeric(bin[,2:13])
    newbin2 <- matrix(newbin,nrow=interval,ncol=12)
    newbin3 <- matrix(nrow=interval,ncol=1)
    for (n in 1:interval)
    {
      newbin3[n,] = sum(newbin2[n,1:12])
    }
    newbin <- cbind(newbin2,newbin3)
    
    col_sum <- sum(table(X$jan))
    whole_sum <- col_sum*12
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
    
    #uncertainty with respect to state H(Y)
    V1 <- newbin[,13]/whole_sum
    V2 <- log10(newbin[,13]/whole_sum)
    for (k in 1:length(V2))
    {
      if(is.finite(V2[k])==F) V2[k] <- 0
      else V2[k] <- V2[k]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- newbin[1:interval,1:12]/whole_sum
    M2 <- log10(M1)
    for (k in 1:length(M2))
    {
      if(is.finite(M2[k])==F) M2[k] <- 0
      else M2[k] <- M2[k]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 12
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output[i, "year"] <- yeare
    output[i,"year_count"] <- col_sum
    output[i,"seasons"] <- whole_sum
    output[i,"HofX"] <- round(HofX,3)
    output[i,"HofY"] <- round(HofY,3)
    output[i,"HofXY"] <- round(HofXY,3)
    output[i,"HXofY"] <- round(HXofY,3)
    output[i,"s"] <- s
    output[i,"t"] <- t
    output[i,"P"] <- round(P,3)
    output[i,"C"] <- round(C,3)
    output[i,"M"] <- round(M,3)
    output[i,"CbyP"] <- round(CoverP,3)
    output[i,"MbyP"] <- round(MoverP,3)
    output[i,"Mutual"] <- round(IofXY,3)
    output[i,"GC"] <- round(GC,3)
    output[i,"C_freedom"] <- C_free
    output[i,"GM"] <- round(GM,3)
    output[i,"M_freedom"] <- M_free
    output[i,"GP"] <- round(GP,3)
    output[i,"P_freedom"] <- P_free
    
  }
  
  write.table(output,outName,sep=",",row.names=F)
}

####################################################################################
##Function to calculate Colwell index for precipitation
##112 years of monthly data, absolute values, 12 bin sizes
##Classification scheme: log scheme

PCM_prec<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
  dir.create(destDir, showWarnings = FALSE)
  ##  inName <- paste(sourceDir, "pre_DF_processed.csv",sep="/")
  inName <- paste(sourceDir, "pre_DF.csv",sep="/")
  outName <- paste(destDir, "pre_PCM.csv", sep="/")
  
  input <- read.table(inName, sep=",", header=T)
  
  temp <- subset(input, year == 1901)
  
  dd <- as.data.frame(input[,5:16])
  
  base.value <- 2.1 ## 2.1^11 = 3503, max 3434
  
  output <- matrix(nrow=nrow(temp),ncol=24)
  output <- as.data.frame(output, row.names = NULL, stringsAsFactors = FALSE)
  colnames(output) <- c("CRU_Site", "lon","lat",
                        "year","year_count","seasons",
                        "HofX","HofY","HofXY","HXofY",
                        "s","t",
                        "P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_freedom","GP","P_freedom")
  
  output[,1:3] <- temp[,1:3]
  
  for (i in 1:nrow(temp))
  {
    
    X <- input[input$CRU_Site == i,]
    
    years <- min(X$year)
    yeare <- max(X$year)
    yearr <- yeare-years+1
    
    interval <- 12
    
    bin <- matrix(0, ncol=14, nrow=interval)
    dimnames(bin) <- list(NULL,c("bin_size","jan","feb","mar","apr","may","jun",
                                 "jul","aug","sep","oct","nov","dec","whole"))
    
    bin[,"bin_size"] <- c("0",base.value^1, base.value^2, base.value^3, 
                          base.value^4, base.value^5, base.value^6,
                          base.value^7, base.value^8, base.value^9, base.value^10,
                          base.value^11)
    
    breaks = c("0","0.00001",base.value^1, base.value^2, base.value^3, 
               base.value^4, base.value^5, base.value^6,
               base.value^7, base.value^8, base.value^9, base.value^10,
               base.value^11)
    
    jan_cut = cut(X$jan, breaks, include.lowest=TRUE,right=TRUE)
    feb_cut = cut(X$feb, breaks, include.lowest=TRUE,right=TRUE)
    mar_cut = cut(X$mar, breaks, include.lowest=TRUE,right=TRUE)
    apr_cut = cut(X$apr, breaks, include.lowest=TRUE,right=TRUE)
    may_cut = cut(X$may, breaks, include.lowest=TRUE,right=TRUE)
    jun_cut = cut(X$jun, breaks, include.lowest=TRUE,right=TRUE)
    jul_cut = cut(X$jul, breaks, include.lowest=TRUE,right=TRUE)
    aug_cut = cut(X$aug, breaks, include.lowest=TRUE,right=TRUE)
    sep_cut = cut(X$sep, breaks, include.lowest=TRUE,right=TRUE)
    oct_cut = cut(X$oct, breaks, include.lowest=TRUE,right=TRUE)
    nov_cut = cut(X$nov, breaks, include.lowest=TRUE,right=TRUE)
    dec_cut = cut(X$dec, breaks, include.lowest=TRUE,right=TRUE)
    
    jan_freq = table(jan_cut)
    feb_freq = table(feb_cut)
    mar_freq = table(mar_cut)
    apr_freq = table(apr_cut)
    may_freq = table(may_cut)
    jun_freq = table(jun_cut)
    jul_freq = table(jul_cut)
    aug_freq = table(aug_cut)
    sep_freq = table(sep_cut)
    oct_freq = table(oct_cut)
    nov_freq = table(nov_cut)
    dec_freq = table(dec_cut)
    
    bin[,"jan"] <- jan_freq
    bin[,"feb"] <- feb_freq
    bin[,"mar"] <- mar_freq
    bin[,"apr"] <- apr_freq
    bin[,"may"] <- may_freq
    bin[,"jun"] <- jun_freq
    bin[,"jul"] <- jul_freq
    bin[,"aug"] <- aug_freq
    bin[,"sep"] <- sep_freq
    bin[,"oct"] <- oct_freq
    bin[,"nov"] <- nov_freq
    bin[,"dec"] <- dec_freq
    
    newbin <- as.numeric(bin[,2:13])
    newbin2 <- matrix(newbin,nrow=interval,ncol=12)
    newbin3 <- matrix(nrow=interval,ncol=1)
    for (n in 1:interval)
    {
      newbin3[n,] = sum(newbin2[n,1:12])
    }
    newbin <- cbind(newbin2,newbin3)
    
    col_sum <- sum(table(X$jan))
    whole_sum <- col_sum*12
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
    
    #uncertainty with respect to state H(Y)
    V1 <- newbin[,13]/whole_sum
    V2 <- log10(newbin[,13]/whole_sum)
    for (k in 1:length(V2))
    {
      if(is.finite(V2[k])==F) V2[k] <- 0
      else V2[k] <- V2[k]
    }
    
    HofY <- -sum(V1*V2)
    
    #uncertainty with respect to interaction of time and state, H(XY)
    M1 <- newbin[1:interval,1:12]/whole_sum
    M2 <- log10(M1)
    for (k in 1:length(M2))
    {
      if(is.finite(M2[k])==F) M2[k] <- 0
      else M2[k] <- M2[k]
    }
    
    HofXY <- -sum(M1*M2)
    
    #Conditional uncertainty with regard to state, with time given, HXofY
    HXofY <- HofXY - HofX
    s <- interval
    t <- 12
    
    #predictability (P), constancy(C) and contingency (M)
    P <- 1-(HXofY/log10(s))
    C <- 1-(HofY/log10(s))
    M <- (HofX+HofY-HofXY)/log10(s)
    CoverP <- C/P
    MoverP <- M/P
    
    #mutual information, I(XY)
    IofXY <- HofY - HXofY
    
    #deviation from homogeneity of the columns of the matrix for constancy, GC
    GC <- 2*whole_sum*(log(s)-HofY)
    C_free <- s-1
    
    #deviation from homogeneity of the columns of the matrix for contingency, GM
    GM <- 2*whole_sum*(HofX+HofY-HofXY)
    M_free <- (s-1)*(t-1)
    
    #deviation from homogeneity of the columns of the matrix for predictability, GP
    GP <- GM + GC
    P_free <- (s-1)*t
    
    output[i, "year"] <- yeare
    output[i,"year_count"] <- col_sum
    output[i,"seasons"] <- whole_sum
    output[i,"HofX"] <- round(HofX,3)
    output[i,"HofY"] <- round(HofY,3)
    output[i,"HofXY"] <- round(HofXY,3)
    output[i,"HXofY"] <- round(HXofY,3)
    output[i,"s"] <- s
    output[i,"t"] <- t
    output[i,"P"] <- round(P,3)
    output[i,"C"] <- round(C,3)
    output[i,"M"] <- round(M,3)
    output[i,"CbyP"] <- round(CoverP,3)
    output[i,"MbyP"] <- round(MoverP,3)
    output[i,"Mutual"] <- round(IofXY,3)
    output[i,"GC"] <- round(GC,3)
    output[i,"C_freedom"] <- C_free
    output[i,"GM"] <- round(GM,3)
    output[i,"M_freedom"] <- M_free
    output[i,"GP"] <- round(GP,3)
    output[i,"P_freedom"] <- P_free
    
  }
  
  write.table(output,outName,sep=",",row.names=F)
}

####################################################################################
## Calculate annual mean for temperature
tempMeans <- function(inFile, outFile) {
  
  tempdf <- read.table(inFile, 
                       header=T, sep=",")
  tempdf$annual_mean <- round(rowMeans(tempdf[,5:16]),2)
  
  outdf <- subset(tempdf, year == 1901)
  outdf <- outdf[,1:4]
  for (i in outdf$CRU_Site)
  {
    outdf[outdf$CRU_Site == i, "annual_mean"] <- round(mean(tempdf[tempdf$CRU_Site == i, "annual_mean"]),2)
  }
  
  write.table(outdf, outFile,
              row.names=F, col.names=T, sep=",")
}

####################################################################################
## Calculate annual mean and annual sums for precipitation
precMeanSums <- function(inFile, outFile) {
  precdf <- read.table(inFile, 
                       header=T, sep=",")
  
  precdf$annual_sum <- round(rowSums(precdf[,5:16]),2)
  precdf$annual_mean <- round(rowMeans(precdf[,5:16]),2)
  
  outdf <- subset(precdf, year == 1901)
  outdf <- outdf[,1:4]
  
  annual_sum<-aggregate(precdf$annual_sum, by = list(precdf$CRU_Site),
                        mean, na.rm = TRUE)
  annual_mean<-aggregate(precdf$annual_mean, by = list(precdf$CRU_Site),
                         mean, na.rm = TRUE)
  
  outdf$annual_sum <- annual_sum[,2]
  outdf$annual_mean <- annual_mean[,2]
  colnames(outdf)<- c("CRU_Site", "lon", "lat", "year", "annual_sum", "annual_mean")
  write.table(outdf, outFile,
              row.names=F, col.names=T, sep=",")
}

####################################################################################
## Project Biome onto CRU PCM data
biomeProject <- function(corFile, tempFile, precFile, pcmFile) {
  
  # temperature PCM with BIOME
  tempPCM <- read.table(tempDF, header=T,sep=",")
  tempDF <- cbind(tempPCM, corDF$BIOME)
  
  colnames(tempDF) <- c("CRU_Site","lon","lat","year","year_count","seasons",
                       "HofX","HofY","HofXY","HXofY","s","t",
                       "P","C","M","CbyP","MbyP",
                       "Mutual","GC","C_freedom","GM","M_Freedom","GP","P_freedom",
                       "BIOME")
  
  # precipitation PCM with BIOME
  precPCM <- read.table(precDF, header=T,sep=",")
  precDF <- cbind(precPCM, corDF$BIOME)
  
  colnames(precDF) <- c("CRU_Site","lon","lat","year","year_count","seasons",
                        "HofX","HofY","HofXY","HXofY","s","t",
                        "P","C","M","CbyP","MbyP",
                        "Mutual","GC","C_freedom","GM","M_Freedom","GP","P_freedom",
                        "BIOME")
  
  myDF <- cbind(tempDF$CRU_Site, tempDF$lon, tempDF$lat, 
                tempDF$P, tempDF$C, tempDF$M, 
                corDF$BIOME,
                precDF$P, precDF$C, precDF$M)
  colnames(myDF) <- c("CRU_Site", "lon", "lat",
                      "tempP", "tempC", "tempM",
                      "BIOME",
                      "precP","precC","precM")
  myDF <- as.data.frame(myDF, stringsASfactors=F)
  
  write.table(myDF, pcmFile, sep=",",
              row.names=F, col.names=T)
  
}

####################################################################################
## Save PCM with prec mean sums, and temp means
matchClimate <- function(tempFile, precFile, pcmFile, fullFile) {
  ## temperature
  tempdf <- read.table(tempFile,
                       header=T,sep=",")
  ## precipitation
  precdf <- read.table(precFile,
                       header=T,sep=",")
  
  ##Process biome specific temp P vs. prec P relationship
  myDF <- read.table(pcmFile, sep=",",
                     header=T)
  
  #myDF$BIOME <- corDF$BIOME
  
  ## Match with CRU_Site reference number
  myDF$temp_annual_mean <- tempdf$annual_mean
  
  myDF$prec_annual_sum <- precdf$annual_sum
  
  myDF$prec_annual_mean <- precdf$annual_mean
  
  
  write.table(myDF, fullFile,
              row.names=F, col.names=T, sep=",")

}

####################################################################################
## Calculate ie factor for temperature and precipitation
iefactor <- function(inDF) {
  
  #IE for temperature
  inDF$tempMC <- inDF$tempM/inDF$tempC
  inDF$tempIE <- ifelse(inDF$tempMC >=1, 2, 1)
  
  #IE for precipitation
  inDF$precMC <- inDF$precM/inDF$precC
  inDF$precIE <- ifelse(inDF$precMC >=1, 2, 1)
  
  return(inDF)
  
}

####################################################################################
## Prepare classes for plotting temp and prec data
classPrep <- function(inDF) {
  
  # prepare breaking points for temperature and precipitation
  temp.lab <- c("<-4.7", "-2.1", "0.5", "3.0", "5.6", "8.2",
                "10.8", "13.4", "16.0", "18.5", "21.1", ">21.1")
  prec.lab <- c("0", "2.3", "5.3", "12.2", "28", "64",
                "148", "340", "783", "1801", "4142",
                ">4142")
  
  # prepare temp and prec label in the dataframe
  plotDF <- inDF
  
  plotDF$templab <- ifelse(plotDF$temp < -4.725, 1, 
                           ifelse(plotDF$temp >= -4.725 & plotDF$temp < -2.14, 2,
                                  ifelse(plotDF$temp >= -2.14 & plotDF$temp < 0.445, 3, 
                                         ifelse(plotDF$temp >= 0.445 & plotDF$temp < 3.03, 4, 
                                                ifelse(plotDF$temp >= 3.03 & plotDF$temp < 5.615, 5, 
                                                       ifelse(plotDF$temp >= 5.615 & plotDF$temp < 8.2, 6, 
                                                              ifelse(plotDF$temp >= 8.2 & plotDF$temp < 10.785, 7,
                                                                     ifelse(plotDF$temp >= 10.785 & plotDF$temp < 13.37, 8, 
                                                                            ifelse(plotDF$temp >= 13.37 & plotDF$temp < 15.955, 9, 
                                                                                   ifelse(plotDF$temp >= 15.955 & plotDF$temp < 18.34, 10,
                                                                                          ifelse(plotDF$temp >= 18.34 & plotDF$temp < 21.125, 11,
                                                                                                 12)))))))))))
  
  plotDF$preclab <- ifelse(plotDF$prec_sum == 0 , 1, 
                           ifelse(plotDF$prec_sum >= 0 & plotDF$prec_sum < 2.3, 2,
                                  ifelse(plotDF$prec_sum >= 2.3 & plotDF$prec_sum < 5.3, 3, 
                                         ifelse(plotDF$prec_sum >= 5.3 & plotDF$prec_sum < 12.2, 4, 
                                                ifelse(plotDF$prec_sum >= 12.2 & plotDF$prec_sum < 28, 5, 
                                                       ifelse(plotDF$prec_sum >= 28 & plotDF$prec_sum < 64, 6, 
                                                              ifelse(plotDF$prec_sum >= 64 & plotDF$prec_sum < 148, 7,
                                                                     ifelse(plotDF$prec_sum >= 148 & plotDF$prec_sum < 340, 8, 
                                                                            ifelse(plotDF$prec_sum >= 340 & plotDF$prec_sum < 783, 9, 
                                                                                   ifelse(plotDF$prec_sum >= 783 & plotDF$prec_sum < 1801, 10,
                                                                                          ifelse(plotDF$prec_sum >= 1801 & plotDF$prec_sum < 4142, 11,
                                                                                                 12)))))))))))
  
  return(plotDF)
}

####################################################################################
## Plotting function 1
PlotMaps <- function(inDF) {
  ## Plot the following maps:
  ## 1. global temperature profile map
  ## 2. global temperature predictability map
  ## 3. global temperature constancy map
  ## 4. global temperautre contigency map
  ## 5. temperature vs. temperature predictability map, grouped by IE factor
  ## 6. Violin plot of the latitudinal patterns of Ie factor
  
  ##### temperature 
  
  # plot temperature mean map
  par(oma=c(1,1,2,2),
      mar=c(5.1,2,4,5))
  with(inDF, quilt.plot(lon, lat, templab, nx=300, ny=260, 
                                 main="Annual mean temperature", add.legend=F))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = temp.lab, title = expression(paste("Temperature [",degree,"C]")),
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
  # plot temperature predictability map
  with(inDF, quilt.plot(lon, lat, tempP, nx=300, ny=260, nlevel=12,
                        main="Temperature predictability", add.legend=T))
  
  # plot temperature constancy map
  with(inDF, quilt.plot(lon, lat, tempC, nx=300, ny=260, nlevel=12,
                        main="Temperature constancy", add.legend=T))
  
  # plot temperature contingency map
  with(inDF, quilt.plot(lon, lat, tempM, nx=300, ny=260, nlevel=12,
                        main="Temperature contingency", add.legend=T))
  
  # plot temperature vs. temperature predictability, grouped by IE factor
  par(oma=c(1,1,1,2),
      mar=c(5,5,4,2))
  with(inDF[inDF$tempIE == 1, ], plot(temp, tempP, 
                                      xlab = expression(paste("Temperature [",degree,"C]")),
                                      ylab = "Temperature predictability",
                                      col=adjustcolor("darkgreen", 0.5),
                                      cex=0.2))
  with(inDF[inDF$tempIE == 2, ], points(temp, tempP,
                                        xlab=NA, ylab=NA, 
                                        col=adjustcolor("lightgreen", 0.5),
                                        cex=0.2))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = c("> 1", "< 1"), title = "Ie factor",
         fill=c("darkgreen", "lightgreen"), cex=1, bty="n")
  par(opar)
  
  # plot Violin plot for latitudinal gradient of temperature Ie and pred
  vioplot(inDF$lat[inDF$tempIE == 1], 
          inDF$lat[inDF$tempIE == 2], names = c("Ie < 1", "Ie > 1"),
          col = "gold")
  title(ylab="Latitude")
  
  ##### precipitation
  
  # plot precipitation map
  par(oma=c(1,1,2,2),
      mar=c(5.1,2,4,5))
  with(inDF, quilt.plot(lon, lat, preclab, nrow=300, ncol=260, 
                                 main="Annual sum precipitation", add.legend=F))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = prec.lab, title = "Precipitation [mm]",
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
  # plot precipitation predictability map
  with(inDF, quilt.plot(lon, lat, precP, nx=300, ny=260, nlevel=12,
                        main="Precipitation predictability", add.legend=T))
  
  # plot Precipitation constancy map
  with(inDF, quilt.plot(lon, lat, precC, nx=300, ny=260, nlevel=12,
                        main="Precipitation constancy", add.legend=T))
  
  # plot Precipitation contingency map
  with(inDF, quilt.plot(lon, lat, precM, nx=300, ny=260, nlevel=12,
                        main="Precipitation contingency", add.legend=T))
  
  # plot Precipitation vs. Precipitation predictability, grouped by IE factor
  par(oma=c(1,1,1,2),
      mar=c(5,5,4,2))
  with(inDF[inDF$precIE == 1, ], plot(prec_sum, precP, 
                                      xlab = "Precipitation",
                                      ylab = "Precipitation predictability",
                                      col=adjustcolor("darkgreen", 0.5),
                                      cex=0.2))
  with(inDF[inDF$precIE == 2, ], points(prec_sum, precP,
                                        xlab=NA, ylab=NA, 
                                        col=adjustcolor("lightgreen", 0.5),
                                        cex=0.2))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = c("> 1", "< 1"), title = "Ie factor",
         fill=c("darkgreen", "lightgreen"), cex=1, bty="n")
  par(opar)
  
  # plot Violin plot for latitudinal gradient of precipitation Ie and pred
  vioplot(inDF$lat[inDF$precIE == 1], 
          inDF$lat[inDF$precIE == 2], names = c("Ie < 1", "Ie > 1"),
          col = "gold")
  title(ylab="Latitude")
}

####################################################################################
## Plotting function 1
PlotPCMMaps <- function(inDF) {
  ## Plot the following maps:
  ## 1. global temperature predictability map
  ## 2. global temperature constancy map
  ## 3. global temperautre contigency map
  ## 4. global prec predictability map
  ## 5. global prec constancy map
  ## 6. global prec contigency map
  
  m <- matrix(c(1,2,3,4,5,6), nrow=3, ncol=2,byrow=T)
  layout(mat=m, heights=c(0.33, 0.33, 0.33))
  
  par(mar=c(2.1,4.1,4.1,2))
  
  ##### temperature 
  # plot temperature predictability map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, tempP, nx=300, ny=260, nlevel=12,
                        main="Temperature", add.legend=T,
                        ylab = "Predictability", cex.lab=1.5,
                        cex.main=1.5))
  mtext("(a)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
  # plot precipitation predictability map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, precP, nx=300, ny=260, nlevel=12,
                        main="Precipitation", add.legend=T, cex.lab=1.5,
                        cex.main=1.5))
  mtext("(b)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
  # plot temperature constancy map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, tempC, nx=300, ny=260, nlevel=12,
                         add.legend=T, ylab = "Constancy", cex.lab=1.5,
                        cex.main=1.5))
  mtext("(c)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
  # plot Precipitation constancy map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, precC, nx=300, ny=260, nlevel=12,
                        add.legend=T, cex.lab=1.5,
                        cex.main=1.5))
  mtext("(d)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
  # plot temperature contingency map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, tempM, nx=300, ny=260, nlevel=12,
                        add.legend=T, ylab = "Contingency", cex.lab=1.5,
                        cex.main=1.5))
  mtext("(e)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
  # plot Precipitation contingency map
  with(inDF[inDF$BIOME <= 14 & inDF$BIOME >= 1,],
       quilt.plot(lon, lat, precM, nx=300, ny=260, nlevel=12,
                        add.legend=T, cex.lab=1.5,
                        cex.main=1.5))
  mtext("(f)", side = 3, adj = 0.05, line = -15.5, cex = 1.5)
  
}

####################################################################################
## Plotting function 1
PlotMapsAnnual <- function(inDF) {
  ## Plot the following maps:
  ## 1. global MAT
  ## 2. global ATP
  
  # plot temperature mean map
  par(oma=c(1,1,2,2),
      mar=c(5.1,2,4,5))
  with(inDF, quilt.plot(lon, lat, templab, nx=300, ny=260, 
                        main="Annual mean temperature", add.legend=F))
  mtext("(a)", side = 3, adj = 0.05, line = -16, cex = 1.5)
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = temp.lab, 
         title = expression(paste("Temperature [",degree,"C]")),
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
  # plot precipitation map
  par(oma=c(1,1,2,2),
      mar=c(5.1,2,4,5))
  with(inDF, quilt.plot(lon, lat, preclab, nrow=300, ncol=260, 
                        main="Annual total precipitation", add.legend=F))
  mtext("(b)", side = 3, adj = 0.05, line = -16, cex = 1.5)
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = prec.lab, title = "Precipitation [mm]",
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
}
####################################################################################
## plotting temp vs. prec all data onto same graph
biomeAll <- function(inDF) {
  ## Plotting biome-specific 2d graphs for:
  ## 1. temperature vs. precipitation
  ## 2. temperature predictability vs. precipitation predictability
  
  # prepare dataframe
  plotDF2 <- subset(inDF, BIOME > 0)
  plotDF2 <- subset(plotDF2, BIOME < 98)
  
  # Setting color.list
  col.list <- palette(rainbow(14))

  # temp vs. prec
  par(oma=c(1,1,1,1),
      mar=c(5.1,5.1,4,12))
  with(plotDF2[plotDF$BIOME == 1, ], plot(temp, prec_sum, 
                                            xlab="Temperature", ylab="Precipitation",
                                            col=adjustcolor(col.list[1],0.5), xlim=c(-30,40), 
                                            ylim=c(0,8000), cex=0.2))
  for(i in 2:14) {
    with(plotDF2[plotDF$BIOME == i, ], points(temp, prec_sum, 
                                            xlab=NA, ylab=NA,
                                            col=adjustcolor(col.list[i],0.5), xlim=c(-30,40), 
                                            ylim=c(0,8000), add=T, cex=0.2))
  }
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = biome, title = "Biome",
         fill=palette(rainbow(14)), cex=1, bty="n")
  par(opar)
  
  # temp P vs. prec P
  par(oma=c(1,1,1,1),
      mar=c(5.1,5.1,4,12))
  with(plotDF2[plotDF$BIOME == 1, ], plot(tempP, precP, 
                                          xlab="Temperature P", ylab="Precipitation P",
                                          col=adjustcolor(col.list[1],0.5), xlim=c(0,1), 
                                          ylim=c(0,1), cex=0.2))
  for(i in 2:14) {
    with(plotDF2[plotDF$BIOME == i, ], points(tempP, precP, 
                                              xlab=NA, ylab=NA,
                                              col=adjustcolor(col.list[i],0.5), xlim=c(0,1), 
                                              ylim=c(0,1), add=T, cex=0.2))
  }
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = biome, title = "Biome",
         fill=palette(rainbow(14)), cex=1, bty="n")
  par(opar)
}

####################################################################################
## plotting bagplot of all data range
biomeBagPlot <- function(inDF) {
  ## Plotting biome-specific 2d graphs for:
  ## 1. temperature vs. precipitation
  ## 2. temperature predictability vs. precipitation predictability
  
  # prepare dataframe
  plotDF2 <- subset(inDF, BIOME > 0)
  plotDF2 <- subset(plotDF2, BIOME < 98)
  
  # Set output graph structure
  set.panel()
  par(oma=c(2,4,2,2),
      mar=c(5.1,5.1,4.1,1.2),
      mgp = c(3, 1, 0))
  set.panel(5,3)
  
  ## Plot temp vs. prec for each biome
  for (i in 1:14) {
    DF <- subset(plotDF2, BIOME == i)
    bagplot(DF$temp, DF$prec_sum, 
            ylab = "Precipitation", xlab = "Temperature", cex.lab=2, 
            xlim = c(-30,40), ylim = c(0,8000),
            main = paste(biome[i]), cex.main=2, show.whiskers=F)
  }
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  
  ## Plot temp P vs. prec P for each biome
  for (i in 1:14) {
    DF <- subset(plotDF2, BIOME == i)
    bagplot(DF$tempP, DF$precP, 
            ylab = "Precipitation predictability", xlab = "Temperature predictability", cex.lab=2, 
            xlim = c(0,1), ylim = c(0,1),
            main = paste(biome[i]), cex.main=2, show.whiskers=F)
  }
  par(opar)
}

####################################################################################
## plotting bagplot of all data range
biomeDensityPlot <- function(inDF) {
  ## Plotting biome-specific density graphs for:
  ## 1. temperature vs. precipitation
  ## 2. temperature predictability vs. precipitation predictability
  
  # prepare dataframe
  plotDF2 <- subset(inDF, BIOME > 0)
  plotDF2 <- subset(plotDF2, BIOME < 98)
  
  # Set output graph structure
  set.panel()
  par(oma=c(2,4,2,2),
      mar=c(5.1,5.1,4.1,1.2),
      mgp = c(3, 1, 0))
  set.panel(5,3)
  
  ## Plot temp vs. prec for each biome
  for (i in 1:14) {
    # subsetting dataframe
    DF <- data.frame(plotDF2[plotDF2$BIOME == i, "temp"],
                     plotDF2[plotDF2$BIOME == i, "prec_sum"])
    # kernel density estimation
    H <- Hpi(x=DF)      # optimal bandwidth estimation
    est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    
    # set contour probabilities for drawing contour levels
    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
         ylab="Precipitation", xlab="Temperature", main=biome[i],
         ylim=c(0,8000), xlim=c(-30,40),las=1) 
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    
  }
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  
  ## Plot temp P vs. prec P for each biome
  for (i in 1:14) {
    # subsetting dataframe
    DF <- data.frame(plotDF2[plotDF2$BIOME == i, "tempP"],
                     plotDF2[plotDF2$BIOME == i, "precP"])
    # kernel density estimation
    H <- Hpi(x=DF)      # optimal bandwidth estimation
    est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    
    # set contour probabilities for drawing contour levels
    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
         ylab="Precipitation predictability", xlab="Temperature predictability", main=biome[i],
         cex.axis=0.75, ylim=c(0,1), xlim=c(0,1),las=1) 
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
  }
  par(opar)
}

####################################################################################
# Process summary dataframe
summaryPrep <- function(inDF) {
  ## Prepare biome specific summary dataframe
  summary <- matrix(nrow = 14, ncol = 17)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","tempP_mean","tempC_mean","tempM_mean",
                         "tempP_sd","tempC_sd","tempM_sd",
                         "precP_mean","precC_mean","precM_mean",
                         "precP_sd","precC_sd","precM_sd",
                         "temp_mean","temp_sd","prec_mean",
                         "prec_sd")
  summary$biome <- c(1:14)
  
  ## read in inDF
  myDF <- inDF
  
  for(i in 1:14) {
    summary[summary$biome == i, "tempP_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempP"]),2)
    summary[summary$biome == i, "tempC_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempC"]),2)
    summary[summary$biome == i, "tempM_mean"] <- round(mean(myDF[myDF$BIOME == i, "tempM"]),2)
    summary[summary$biome == i, "precP_mean"] <- round(mean(myDF[myDF$BIOME == i, "precP"]),2)
    summary[summary$biome == i, "precC_mean"] <- round(mean(myDF[myDF$BIOME == i, "precC"]),2)
    summary[summary$biome == i, "precM_mean"] <- round(mean(myDF[myDF$BIOME == i, "precM"]),2)
    summary[summary$biome == i, "tempP_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempP"]),2)
    summary[summary$biome == i, "tempC_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempC"]),2)
    summary[summary$biome == i, "tempM_sd"] <- round(sd(myDF[myDF$BIOME == i, "tempM"]),2)
    summary[summary$biome == i, "precP_sd"] <- round(sd(myDF[myDF$BIOME == i, "precP"]),2)
    summary[summary$biome == i, "precC_sd"] <- round(sd(myDF[myDF$BIOME == i, "precC"]),2)
    summary[summary$biome == i, "precM_sd"] <- round(sd(myDF[myDF$BIOME == i, "precM"]),2)
    summary[summary$biome == i, "temp_mean"] <- round(mean(myDF[myDF$BIOME == i, "temp"]),2)
    summary[summary$biome == i, "prec_mean"] <- round(mean(myDF[myDF$BIOME == i, "prec_sum"]),2)
    summary[summary$biome == i, "temp_sd"] <- round(sd(myDF[myDF$BIOME == i, "temp"]),2)
    summary[summary$biome == i, "prec_sd"] <- round(sd(myDF[myDF$BIOME == i, "prec_sum"]),2)
  }
  
  return(summary)
}

####################################################################################
# Process summary dataframe
summaryPrep_min <- function(inDF) {
  ## Prepare biome specific summary dataframe
  summary <- matrix(nrow = 14, ncol = 9)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","tempP_min","tempC_min","tempM_min",
                         "precP_min","precC_min","precM_min",
                         "temp_min","prec_min")
  summary$biome <- c(1:14)
  
  ## read in inDF
  myDF <- inDF
  
  for(i in 1:14) {
    summary[summary$biome == i, "tempP_min"] <- round(min(myDF[myDF$BIOME == i, "tempP"]),2)
    summary[summary$biome == i, "tempC_min"] <- round(min(myDF[myDF$BIOME == i, "tempC"]),2)
    summary[summary$biome == i, "tempM_min"] <- round(min(myDF[myDF$BIOME == i, "tempM"]),2)
    summary[summary$biome == i, "precP_min"] <- round(min(myDF[myDF$BIOME == i, "precP"]),2)
    summary[summary$biome == i, "precC_min"] <- round(min(myDF[myDF$BIOME == i, "precC"]),2)
    summary[summary$biome == i, "precM_min"] <- round(min(myDF[myDF$BIOME == i, "precM"]),2)
    summary[summary$biome == i, "temp_min"] <- round(min(myDF[myDF$BIOME == i, "temp"]),2)
    summary[summary$biome == i, "prec_min"] <- round(min(myDF[myDF$BIOME == i, "prec_sum"]),2)
  }
  
  return(summary)
}

####################################################################################
# Process summary dataframe
summaryPrep_max <- function(inDF) {
  ## Prepare biome specific summary dataframe
  summary <- matrix(nrow = 14, ncol = 9)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","tempP_max","tempC_max","tempM_max",
                         "precP_max","precC_max","precM_max",
                         "temp_max","prec_max")
  summary$biome <- c(1:14)
  
  ## read in inDF
  myDF <- inDF
  
  for(i in 1:14) {
    summary[summary$biome == i, "tempP_max"] <- round(max(myDF[myDF$BIOME == i, "tempP"]),2)
    summary[summary$biome == i, "tempC_max"] <- round(max(myDF[myDF$BIOME == i, "tempC"]),2)
    summary[summary$biome == i, "tempM_max"] <- round(max(myDF[myDF$BIOME == i, "tempM"]),2)
    summary[summary$biome == i, "precP_max"] <- round(max(myDF[myDF$BIOME == i, "precP"]),2)
    summary[summary$biome == i, "precC_max"] <- round(max(myDF[myDF$BIOME == i, "precC"]),2)
    summary[summary$biome == i, "precM_max"] <- round(max(myDF[myDF$BIOME == i, "precM"]),2)
    summary[summary$biome == i, "temp_max"] <- round(max(myDF[myDF$BIOME == i, "temp"]),2)
    summary[summary$biome == i, "prec_max"] <- round(max(myDF[myDF$BIOME == i, "prec_sum"]),2)
  }
  
  return(summary)
}
####################################################################################
# Plotting summary profile using 2-dimensional error bars
summary2d <- function(summary) {
  
  # setting graphics
  require(psych)
  m <- matrix(c(1,2,3,3), nrow=2, ncol=2,byrow=T)
  layout(mat=m, heights=c(0.8,0.2))
  
  ## temp vs. prec
  df1 <- data.frame(mean=summary$temp_mean, sd=summary$temp_sd)
  df2 <- data.frame(mean=summary$prec_mean, sd=summary$prec_sd)
  error.crosses(df1, df2, sd=T, color=color.list, xlab="Temperature",
                ylab="Precipitation", main=NA)
  ## tempP vs. precP
  df1 <- data.frame(mean=summary$tempP_mean, sd=summary$tempP_sd)
  df2 <- data.frame(mean=summary$precP_mean, sd=summary$precP_sd)
  error.crosses(df1, df2, sd=T, color=color.list,
                xlab = "Temperature P", ylab = "Precipitation P",
                main=NA)
  par(mar = rep(2, 4))
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
         cex=0.8,ncol=4)
  
  ## temp vs. prec
  with(summary, plot(temp_mean, prec_mean, col=color.list,
                     pch=19, xlim=c(-20,40), ylim=c(0,4000),
                     xlab="Temperature", ylab = "Precipitation"))
  
  ## tempP vs. precP
  with(summary, plot(tempP_mean, precP_mean, col=color.list,
                     pch=19, xlim=c(0,1), ylim=c(0,1),
                     xlab="Temperature P", ylab = "Precipitation P"))
  
  par(mar = rep(2, 4))
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend(x="bottom",inset=0, legend = biome, fill=color.list, 
         cex=0.8,ncol=4)
  par(opar)
}

####################################################################################
## Prepare 50th percentile data for temp vs. prec dataframe
Percentile50_absolute <- function(inDF) {
  ## read in file
  myDF <- inDF
  
  ## setting summary dataframe
  summary <- matrix(nrow = 14, ncol = 4)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","temp_mean","prec_mean","dist_50")
  summary$biome <- c(1:14)
  
  ## Filling biome-specific average values
  for (i in 1:14) {
    summary[summary$biome == i, "temp_mean"] <- mean(myDF[myDF$BIOME == i, "temp"])
    summary[summary$biome == i, "prec_mean"] <- mean(myDF[myDF$BIOME == i, "prec_sum"])
  }
  
  ## Calculating distances
  i <- 1
  
  newDF <- subset(myDF, BIOME == i)
  temp <- summary[summary$biome == i, "temp_mean"]
  prec <- summary[summary$biome == i, "prec_mean"]
  newDF$dist <- sqrt((temp - newDF$temp)^2 + 
                       (prec - newDF$prec_sum)^2)
  output <- newDF
  
  for (i in 2:14)
  {
    newDF <- subset(myDF, BIOME == i)
    temp <- summary[summary$biome == i, "temp_mean"]
    prec <- summary[summary$biome == i, "prec_mean"]
    newDF$dist <- sqrt((temp - newDF$temp)^2 + 
                         (prec - newDF$prec_sum)^2)
    
    output <- rbind(output, newDF)
  }
  
  ## calculating the quantiles and the subset
  
  for (i in 1:14)
  {
    summary[summary$biome == i, "dist_50"] <- quantile(output[output$BIOME == i, "dist"], 0.5)
    output[output$BIOME == i, "subset"] <- summary[summary$biome == i, "dist_50"] - output[output$BIOME == i, "dist"]
    
  }
  
  ## subset output with distance smaller than 50th percentile
  subDF <- subset(output, subset >= 0)
  
  return(subDF)
}


####################################################################################
## Prepare 50th percentile data for temp vs. prec P dataframe
Percentile50_p <- function(inDF) {
  
  ## read in file
  myDF <- inDF
  
  ## setting summary dataframe
  summary <- matrix(nrow = 14, ncol = 4)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","temp_mean","prec_mean","dist_50")
  summary$biome <- c(1:14)
  
  ## Filling biome-specific average values
  for (i in 1:14) {
    summary[summary$biome == i, "temp_mean"] <- mean(myDF[myDF$BIOME == i, "tempP"])
    summary[summary$biome == i, "prec_mean"] <- mean(myDF[myDF$BIOME == i, "precP"])
  }
  
  ## Calculating distances
  i <- 1
  
  newDF <- subset(myDF, BIOME == i)
  temp <- summary[summary$biome == i, "temp_mean"]
  prec <- summary[summary$biome == i, "prec_mean"]
  newDF$dist <- sqrt((temp - newDF$tempP)^2 + 
                       (prec - newDF$precP)^2)
  output <- newDF
  
  for (i in 2:14)
  {
    newDF <- subset(myDF, BIOME == i)
    temp <- summary[summary$biome == i, "temp_mean"]
    prec <- summary[summary$biome == i, "prec_mean"]
    newDF$dist <- sqrt((temp - newDF$tempP)^2 + 
                         (prec - newDF$precP)^2)
    
    output <- rbind(output, newDF)
  }
  
  ## calculating the quantiles and the subset
  for (i in 1:14)
  {
    summary[summary$biome == i, "dist_50"] <- quantile(output[output$BIOME == i, "dist"], 0.5)
    output[output$BIOME == i, "subset"] <- summary[summary$biome == i, "dist_50"] - output[output$BIOME == i, "dist"]
    
  }
  
  ## subset output with distance smaller than 50th percentile
  subDF <- subset(output, subset >= 0)
  
  return(subDF)
}

####################################################################################
## Prepare 50th percentile data for temp vs. prec C dataframe
Percentile50_c <- function(inDF) {
  
  ## read in file
  myDF <- inDF
  
  ## setting summary dataframe
  summary <- matrix(nrow = 14, ncol = 4)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","temp_mean","prec_mean","dist_50")
  summary$biome <- c(1:14)
  
  ## Filling biome-specific average values
  for (i in 1:14) {
    summary[summary$biome == i, "temp_mean"] <- mean(myDF[myDF$BIOME == i, "tempC"])
    summary[summary$biome == i, "prec_mean"] <- mean(myDF[myDF$BIOME == i, "precC"])
  }
  
  ## Calculating distances
  i <- 1
  
  newDF <- subset(myDF, BIOME == i)
  temp <- summary[summary$biome == i, "temp_mean"]
  prec <- summary[summary$biome == i, "prec_mean"]
  newDF$dist <- sqrt((temp - newDF$tempC)^2 + 
                       (prec - newDF$precC)^2)
  output <- newDF
  
  for (i in 2:14)
  {
    newDF <- subset(myDF, BIOME == i)
    temp <- summary[summary$biome == i, "temp_mean"]
    prec <- summary[summary$biome == i, "prec_mean"]
    newDF$dist <- sqrt((temp - newDF$tempC)^2 + 
                         (prec - newDF$precC)^2)
    
    output <- rbind(output, newDF)
  }
  
  ## calculating the quantiles and the subset
  for (i in 1:14)
  {
    summary[summary$biome == i, "dist_50"] <- quantile(output[output$BIOME == i, "dist"], 0.5)
    output[output$BIOME == i, "subset"] <- summary[summary$biome == i, "dist_50"] - output[output$BIOME == i, "dist"]
    
  }
  
  ## subset output with distance smaller than 50th percentile
  subDF <- subset(output, subset >= 0)
  
  return(subDF)
}

####################################################################################
## Prepare 50th percentile data for temp vs. prec M dataframe
Percentile50_m <- function(inDF) {
  
  ## read in file
  myDF <- inDF
  
  ## setting summary dataframe
  summary <- matrix(nrow = 14, ncol = 4)
  summary <- as.data.frame(summary)
  colnames(summary) <- c("biome","temp_mean","prec_mean","dist_50")
  summary$biome <- c(1:14)
  
  ## Filling biome-specific average values
  for (i in 1:14) {
    summary[summary$biome == i, "temp_mean"] <- mean(myDF[myDF$BIOME == i, "tempM"])
    summary[summary$biome == i, "prec_mean"] <- mean(myDF[myDF$BIOME == i, "precM"])
  }
  
  ## Calculating distances
  i <- 1
  newDF <- subset(myDF, BIOME == i)
  temp <- summary[summary$biome == i, "temp_mean"]
  prec <- summary[summary$biome == i, "prec_mean"]
  newDF$dist <- sqrt((temp - newDF$tempM)^2 + 
                       (prec - newDF$precM)^2)
  output <- newDF
  
  for (i in 2:14)
  {
    newDF <- subset(myDF, BIOME == i)
    temp <- summary[summary$biome == i, "temp_mean"]
    prec <- summary[summary$biome == i, "prec_mean"]
    newDF$dist <- sqrt((temp - newDF$tempM)^2 + 
                         (prec - newDF$precM)^2)
    
    output <- rbind(output, newDF)
  }
  
  ## calculating the quantiles and the subset
  for (i in 1:14)
  {
    summary[summary$biome == i, "dist_50"] <- quantile(output[output$BIOME == i, "dist"], 0.5)
    output[output$BIOME == i, "subset"] <- summary[summary$biome == i, "dist_50"] - output[output$BIOME == i, "dist"]
    
  }
  
  ## subset output with distance smaller than 50th percentile
  subDF <- subset(output, subset >= 0)
  
  return(subDF)
}

####################################################################################
## plot 50th percentile data
plot50th <- function(absDF, pDF, raw) {
  
  ## setting summary dataframe
  abs <- matrix(nrow = 14, ncol = 5)
  abs <- as.data.frame(abs)
  colnames(abs) <- c("biome","temp_mean","prec_mean",
                     "temp_p", "prec_p")
  abs$biome <- c(1:14)
  
  ## Filling biome-specific average values
  for (i in 1:14) {
    abs[abs$biome == i, "temp_mean"] <- mean(raw[raw$BIOME == i, "temp"])
    abs[abs$biome == i, "prec_mean"] <- mean(raw[raw$BIOME == i, "prec_sum"])
    abs[abs$biome == i, "temp_p"] <- mean(raw[raw$BIOME == i, "tempP"])
    abs[abs$biome == i, "prec_p"] <- mean(raw[raw$BIOME == i, "precP"])
  }
  
  # Set output graph structure
  set.panel()
  par(oma=c(2,4,2,2),
      mar=c(5.1,5.1,4.1,1.2),
      mgp = c(3, 1, 0))
  set.panel(5,3)
  
  ## abs climate 50th percentile
  for (i in 1:14) {
    with(absDF[absDF$BIOME == i, ], plot(temp, prec_sum, xlab="Temperature",
                                         ylab="Precipitation", xlim=c(-30,30), main=biome[i],
                                         ylim=c(0, 4000), col=adjustcolor(color.list[i], 0.5),
                                         pch=21))
    with(abs[abs$biome == i, ], points(temp_mean, prec_mean, pch=4, cex=4,
                                     xlim=c(-30,30),ylim=c(0,4000),
                                     col="black"))
  }
  
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  
  ## climatic predictability 50th percentile
  for (i in 1:14) {
    with(absDF[absDF$BIOME == i, ], plot(tempP, precP, xlab="Temperature P",
                                           ylab="Precipitation P", xlim=c(0,1), main=biome[i],
                                           ylim=c(0, 1), col=adjustcolor(color.list[i], 0.5),
                                           pch=21))
    with(abs[abs$biome == i, ], points(temp_p, prec_p, pch=4, cex=4,
                                       xlim=c(0,1),ylim=c(0,1),
                                       col="black"))
  }
  par(opar)

} 

####################################################################################
## Plot radar plots
radar_summary <- function(inDF) {
  
  # library
  require(fmsb)
  
  # prepare dataframe
  col.list <- color.list
  newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                      inDF$tempP_mean,inDF$precP_mean)
  names(newDF) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF) <- biome
  
  # plotting
  for(i in 1:14) {
    plotDF <- rbind(c(30, 2500, 1, 1), c(-20, 0, 0, 0), newDF[i,])
    radarchart(plotDF, pcol=col.list[i], pfcol=adjustcolor(col.list[i],0.2))
    title(biome[i])
  }  

}

####################################################################################
## Plot radar plots
radar_summary_image <- function(inDF1, inDF2) {
  
  # library
  require(fmsb)
  
  # prepare dataframe
  col.list <- color.list
  newDF1 <- data.frame(inDF1$temp_max, inDF1$prec_max,
                      inDF1$tempP_max,inDF1$precP_max)
  names(newDF1) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF1) <- biome
  
  newDF2 <- data.frame(inDF2$temp_min, inDF2$prec_min,
                       inDF2$tempP_min,inDF2$precP_min)
  names(newDF2) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF2) <- biome
  
  par(mfrow=c(5,3))
  
  # plotting
  for(i in 1:14) {
    plotDF <- rbind(c(35, 7500, 1, 1), c(-30, 0, 0, 0), newDF1[i,], newDF2[i,])
    radarchart(plotDF, pcol=c(col.list[i],adjustcolor("white",0)),axistype=2,
               pfcol=c(adjustcolor(col.list[i],0.2),adjustcolor("white",0.8)),
               axislabcol="black", vlcex = 1.2, caxislabels = c(-30, 0, 0, 0),
               paxislabels=c(35, 7500, 1, 1),cglwd=0.8)
    title(biome[i])
  }  
  par(opar)
}

####################################################################################
## Plot radar plots
radar_summary_image2 <- function(inDF) {
  
  # library
  require(fmsb)
  
  # prepare dataframe
  col.list <- color.list
  
  newDF2 <- data.frame(inDF$temp_mean, inDF$prec_mean,
                       inDF$tempP_mean,inDF$precP_mean)
  names(newDF2) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF2) <- biome
  
  op <- par(mfrow=c(5,3),
            mar=c(1,0.2,1.5,0.2))
  
  spider(y=1:14, x=1:4, data=newDF2, rescale=T, 
          main = biome, connect=T, ncol=1)

  par(op)
}

####################################################################################
## Plot star plots
star_summary_image <- function(inDF) {
  
  # library
  require(fmsb)
  
  # prepare dataframe
  col.list <- color.list
  newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                      inDF$tempP_mean,inDF$precP_mean)
  names(newDF) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF) <- biome
  
  # plotting
  stars(newDF, len = 0.8, key.loc = c(6.8, 2.3),
         draw.segments = T, 
        nrow=5, ncol=3,
        col.segments=c("red","blue","yellow","orange"))
  
}
####################################################################################
## kernel density plot
kernel_multi <- function(inDF) {
  
  newDF <- inDF
  # library
  require(sm)
  
  # set plot structure
  m <- matrix(c(1,2,3,4,5,5), nrow=3, ncol=2,byrow=T)
  layout(mat=m, heights=c(0.4, 0.4, 0.2))
  
  ## temperature
  sm.density.compare(newDF$temp, newDF$BIOME, xlab="temperature")

  ## precipitation
  sm.density.compare(newDF$prec_sum, newDF$BIOME, xlab="precipitation")

  ## temperature predictability
  sm.density.compare(newDF$tempP, newDF$BIOME, xlab="temperature predictability")

  ## precipitation predictability
  sm.density.compare(newDF$precP, newDF$BIOME, xlab="precipitation predictability")
  
  colfill<-c(2:15)

  par(mar = rep(2, 4))
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend(x="bottom",inset=0, legend = biome, fill=colfill, 
         cex=0.8,ncol=4)
  par(opar)
  
}

####################################################################################
##Plot 2d environmental information factor for each biome
Ieplot <- function(inDF) {
  
  # library
  require(lattice)
  require(gridExtra)

  # read in file
  myDF <- inDF
  
  # process each biome
  for (i in 1:14) {
    # set plot structure
    m <- matrix(c(1,2,3), nrow=3, ncol=1, byrow=T)
    layout(mat=m, heights=c(0.4, 0.4, 0.2))
    
    # temperature vs. temperature predictability
    with(myDF[myDF$BIOME == i & myDF$tempIE == 1, ], plot(temp, tempP, xlab="Temperature",
                                                          ylab = "Temperature predictability",
                                                          main = biome[i], xlim = c(-40,30),
                                                          cex = 0.2, ylim = c(0,1),
                                                          col=adjustcolor("green", 0.5)))
    with(myDF[myDF$BIOME == i & myDF$tempIE == 2, ], points(temp, tempP, xlab=NA, ylab=NA,
                                                            xlim=c(-40,30),
                                                            ylim=c(0,1), cex = 0.2,
                                                            col=adjustcolor("blue", 0.5)))
    
    # precipitation vs. precipitation predictability
    with(myDF[myDF$BIOME == i & myDF$tempIE == 1, ], plot(prec_sum, precP, xlab="Precipitation",
                                                          ylab="Precipitation predictability",
                                                            main = biome[i], xlim=c(0,8000),
                                                            ylim=c(0,1), cex = 0.2,
                                                            col=adjustcolor("green", 0.5)))
    with(myDF[myDF$BIOME == i & myDF$tempIE == 2, ], points(prec_sum, precP, xlab="Precipitation",
                                                          ylab="Precipitation predictability",
                                                          xlim=c(0,8000),
                                                          ylim=c(0,1), cex = 0.2,
                                                          col=adjustcolor("blue", 0.5)))
    par(mar = rep(2, 4))
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(x="bottom",inset=0, legend = c("Ie < 1", "Ie > 1"), fill=c("green", "blue"), 
           cex=1.5, horiz=T)
    par(opar)
  }
  
}

####################################################################################
## Plot climate space using hexagon   -- NOT USED

ClimSpace <- function(inDF) {
  
  # library
  require(hexbin)
  
  # read in file
  newDF <- inDF
  
  # temperature vs. precipitation
  hbin <- hexbin(newDF$temp, newDF$prec_sum, xbins=40,
                 xlab="temperature", ylab="precipitation")
  plot(hbin)
  
  # temperature pred vs. precipitation pred
  hbin <- hexbin(newDF$tempP, newDF$precP, 
                 xlab="temperature predictability",
                 ylab="precipitation predictability", xbins=40)
  plot(hbin)
  
  # temperature vs. temp pred
  hbin <- hexbin(newDF$temp_annual_mean, newDF$tempP, 
                 xlab="temperature",
                 ylab="temperature predictability", xbins=40)
  plot(hbin)
  
  # prec vs. prec pred
  hbin <- hexbin(newDF$prec_annual_sum, newDF$precP, 
                 xlab="precipitation",
                 ylab="precipitation predictability", xbins=40)
  p<-plot(hbin)
  pushHexport(p$plot.vp)
  
  # add points
  grid.points(1000, 0.4, pch=16, gp=gpar(col="red"))
  upViewport()
  
}

####################################################################################
##PCA kernel density analysis using summary statistics

SummaryPCA <- function(inDF) {
  
  # library
  require(ks)
  require(vegan)
  
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

####################################################################################
##PCA kernel density analysis using summary statistics

SummaryPCA_image <- function(inDF) {
  
  # library
  require(ks)
  require(vegan)
  
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
  #plot(prin, main = "Importance of components")
  
  # Assign principal component values onto x and y axises
  x.lab <- paste("PC1 [", round(PoV[[1]]*100, 0), "%]", sep="")
  y.lab <- paste("PC2 [", round(PoV[[2]]*100, 0), "%]", sep="")
  
  # plotting
  biplot(prin, xlab = x.lab, ylab= y.lab,
         col=c("red","blue"))
}

####################################################################################
##PCA kernel density analysis using all data
BiomePCA <- function(inDF) {
  
  # library
  require(vegan)
  require(ks)
  
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
    
    ################ KERNEL DENSITY ESTIMATION ##############################
    H <- Hpi(x=pc12)      # optimal bandwidth estimation
    est<- kde(x=pc12, H=H, compute.cont=TRUE)     # kernel density estimation
    
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
  
  return(outDF)
}

####################################################################################
##PCA kernel density analysis using all data, regardless of biome
TotalPCA <- function(inDF) {
  
  # library
  require(vegan)
  require(ks)
  
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
  
  ################ KERNEL DENSITY ESTIMATION ##############################
  H <- Hpi(x=pc12)      # optimal bandwidth estimation
  est<- kde(x=pc12, H=H, compute.cont=TRUE)     # kernel density estimation
  
  # set contour probabilities for drawing contour levels
  cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
  
  fit<-envfit(pc12, dat2) # use envfit for drawing arrows, can be also done using trait loadings
  fit2<-fit$vectors$arrows*-1 # drawing line segments in arrow opposites direction for pretty layout
  
  plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, ylab=y.lab, xlab=x.lab, 
       cex.axis=0.75, ylim=c(-10, 10), xlim=c(-10, 10),las=1) 
  plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
  plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
  plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
  plot(fit, cex=0.90, col=1, labels=list(vectors = c("temp", "prec", "temp P", "prec P")))

  return(myDF)
}
####################################################################################
## plot 50th percentile spatial data
spatial50 <- function(absDF, pDF) {
  
  # Set output graph structure
  par(oma=c(2,2,2,2),
     mar=c(5.1,5.1,5.1,5.1),
     mgp = c(3, 1, 0))
  set.panel(2,2)
  
  ## abs climate 50th percentile
  with(absDF, quilt.plot(lon, lat, temp, main="Mean annual temperature",
                         nx=300, ny=260, nlevel=12))
  world(add=T, col=adjustcolor("grey", 0.5))
  with(absDF, quilt.plot(lon, lat, prec_sum, main="Sum annual precipitation",
                         nx=300, ny=260, nlevel=12))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  ## predictability climate 50th percentile
  with(pDF, quilt.plot(lon, lat, tempP, main="Temperature predictability",
                       nx=300, ny=260, nlevel=12))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  with(pDF, quilt.plot(lon, lat, precP, main="Precipitation predictability",
                       nx=300, ny=260, nlevel=12))
  world(add=T, col=adjustcolor("grey", 0.5))
} 

####################################################################################
## Read in NDVI data and NDVI predictability data

ndvi_to_cru <- function(cruDF, outPath) {
  
  # library
  require(sp)
  require(rgdal)
  
  # in file path
  inPath <- "/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/NDVI"
  
  # NDVI data
  ndvi <- read.table(paste(inPath, "/NDVI_DF.csv", sep=""), 
                     header=T, sep = ",")
  
  # row means
  ndvi$mean <- round(rowMeans(ndvi[,5:16]),2)
  
  # calculate site averages
  newDF <- aggregate(ndvi$mean, by = list(ndvi$Site),
                     mean, na.rm = TRUE)
  
  # NDVI PCM
  ndvip <-read.table(paste(inPath, "/PCM/NDVI_DF.csv", sep=""), 
                     header=T, sep = ",")
  
  ndvip2 <- ndvip[order(ndvip$Site),]
  newDF2 <- newDF[order(newDF$Group.1),]
  
  ndvip <- ndvip2
  newDF <- newDF2
  
  outDF <- data.frame(ndvip$Site, ndvip$lat, ndvip$lon,
                      ndvip$P, ndvip$C, ndvip$M, newDF2$x)
  colnames(outDF) <- c("Site", "lat", "lon", "ndviP",
                       "ndviC", "ndviM", "ndviMean")
  
  # Conver NDVI coordinates to CRU coordinates
  outDF1 <- subset(outDF, lon <= 180)
  outDF2 <- subset(outDF, lon > 180)
  outDF2$lon <- (outDF2$lon - 360)
  newout <- rbind(outDF1, outDF2)
  
  # read in cru dataframe
  corDF <- data.frame(cruDF$lon, cruDF$lat, cruDF$CRU_Site)
  colnames(corDF) <- c("lon", "lat", "CRU_Site")
  
  # convert cru into raster
  coordinates(corDF) <- ~lon+lat
  gridded(corDF) <- T
  r <- raster(corDF)
  
  # convert ndvi P into raster
  subDF <- data.frame(newout$lon, newout$lat, newout$ndviP)
  colnames(subDF) <- c("lon", "lat", "ndviP")
  coordinates(subDF) <- ~lon+lat
  gridded(subDF) <- T
  p <- raster(subDF)
  
  # resampling ndvi P onto cru points
  res <- resample(p, r, method="ngb")
  out <-   as.data.frame(rasterToPoints(res))
  
  # convert ndvi C into raster
  subDF <- data.frame(newout$lon, newout$lat, newout$ndviC)
  colnames(subDF) <- c("lon", "lat", "ndviC")
  coordinates(subDF) <- ~lon+lat
  gridded(subDF) <- T
  p <- raster(subDF)
  
  # resampling ndvi C onto cru points
  res <- resample(p, r, method="ngb")
  out1 <-   as.data.frame(rasterToPoints(res))
  
  # convert ndvi M into raster
  subDF <- data.frame(newout$lon, newout$lat, newout$ndviM)
  colnames(subDF) <- c("lon", "lat", "ndviM")
  coordinates(subDF) <- ~lon+lat
  gridded(subDF) <- T
  p <- raster(subDF)
  
  # resampling ndvi M onto cru points
  res <- resample(p, r, method="ngb")
  out2 <-   as.data.frame(rasterToPoints(res))
  
  # convert ndvi_mean into raster
  subDF <- data.frame(newout$lon, newout$lat, newout$ndviMean)
  colnames(subDF) <- c("lon", "lat", "ndviMean")
  coordinates(subDF) <- ~lon+lat
  gridded(subDF) <- T
  p <- raster(subDF)
  
  # resampling ndvi_mean onto cru points
  res <- resample(p, r, method="ngb")
  out3 <-  as.data.frame(rasterToPoints(res))
  
  # combine ndvi output at CRU grids
  out <- data.frame(out, out1$ndviC, 
                    out2$ndviM, out3$ndviMean)
  colnames(out) <- c("lon", "lat", "ndviP",
                     "ndviC", "ndviM", "ndviMean")
  
  # prepare lon lat list to project out DF onto CRU DF
  lon.list <- unique(out$lon)
  lat.list <- unique(out$lat)
  for (i in lon.list) {
    for (j in lat.list) {
      grid.check <- length(out[out$lon == i & out$lat == j, "ndviP"])
      
      cruDF[cruDF$lon == i & cruDF$lat == j, "ndviP"] <- ifelse(grid.check > 0, 
                                                                out[out$lon == i & out$lat == j, "ndviP"],
                                                                NA)
                                                                
      cruDF[cruDF$lon == i & cruDF$lat == j, "ndviC"] <- ifelse(grid.check > 0, 
                                                                out[out$lon == i & out$lat == j, "ndviC"],
                                                                NA)
      cruDF[cruDF$lon == i & cruDF$lat == j, "ndviM"] <- ifelse(grid.check > 0, 
                                                                out[out$lon == i & out$lat == j, "ndviM"],
                                                                NA)
      cruDF[cruDF$lon == i & cruDF$lat == j, "ndviMean"] <- ifelse(grid.check > 0, 
                                                                   out[out$lon == i & out$lat == j, "ndviMean"],
                                                                   NA)
    }
  }
  
  write.table(cruDF, outPath, col.names=T, row.names=F, sep=",")
  
}

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


####################################################################################
## Process species richness data onto CRU grids

richnessCRU <- function(inFile, outFile) {
  # Species richness data source: http://www.biodiversitymapping.org/download.htm  
  
  # library
  require(raster)
  require(rgdal)
  
  # read in CRU data and extract grid information
  cruDF <- read.table(paste(destDir, "/biome_temp_prec_full.csv", sep=""), 
                      header=T,sep=",")
  corDF <- data.frame(cruDF$lon, cruDF$lat, cruDF$CRU_Site)
  colnames(corDF) <- c("lon", "lat", "CRU_Site")
  
  # prepare output dataframe
  outDF <- corDF
  
  # convert cru into raster
  coordinates(corDF) <- ~lon+lat
  gridded(corDF) <- T
  cru <- raster(corDF)
  
  # prepare species class list
  class.list <- c("Amphibians", "Birds", "ConeSnails", "Mammals")
  class.short <- c("A_", "B_", "C_", "M_")
  
  ## Read in species richness data
  for (c.name in 1:length(class.list)) {
    
    # prepare directory paths
    sourceDir <- paste(inFile, "/", class.list[c.name],sep="")
    DatFiles <- list.files(path = sourceDir, pattern = "\\.tif")
    
    for (thisFile in 1:length(DatFiles)) {
      inName <- file.path(sourceDir, DatFiles[thisFile], fsep = .Platform$file.sep)
      
      # prepare colname
      sub1 <- substr(DatFiles[thisFile], 15, nchar(DatFiles[thisFile]))
      sub2 <- substr(sub1,1,nchar(sub1)-11)
      colname <- paste(class.short[c.name], sub2, sep="")
      
      # read in raster file
      r <-raster(inName)
      
      # convert to points
      spts <- rasterToPoints(r, spatial = TRUE)
      
      # make projections and transformation
      llprj <-  "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
      llpts <- spTransform(spts, CRS(llprj))
      
      # save as data frame
      pnts <- as.data.frame(llpts)
      
      # rasterize the point data
      e <- extent(pnts[,2:3])
      ra <- raster(e, ncol=3329, nrow=1300)
      newR <- rasterize(pnts[,2:3], ra, pnts[,1], fun=mean)
      
      # resample species richness data based on CRU dataframe
      res <- resample(newR, cru, method="ngb")
      out <- as.data.frame(rasterToPoints(res))
      
      # prepare lon lat list to project out DF onto CRU DF
      lon.list <- unique(out$x)
      lat.list <- unique(out$y)
      for (i in lon.list) {
        for (j in lat.list) {
          grid.check <- length(out[out$x == i & out$y == j, "layer"])
          
          outDF[outDF$lon == i & outDF$lat == j, colname] <- ifelse(grid.check > 0, 
                                                               out[out$x == i & out$y == j, "layer"],
                                                               NA)
        } ## for i list 
      }  # for j list
      
    } ## for thisFile list
  } ## species class list
  
  write.table(outDF, outFile, col.names=T, row.names=F, sep=",")
}

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

####################################################################################
## Function within fiaPrep:
## Project fia data onto cru data points
## Make a list of variables to be saved onto CRU dataframe
CRUcor <- function(inDF) {
  inDF$intlon <- round(inDF[,"LON"], 0)
  inDF$declon <- inDF[,"LON"] - inDF$intlon
  inDF$u <- inDF$intlon + 0.25
  inDF$d <- inDF$intlon + 0.75
  inDF[,"LON_CRU"] <- ifelse(inDF[,"LON"] < inDF[,"u"], inDF$intlon-0.25,
                             ifelse(inDF[, "LON"] >= inDF[,"d"], (inDF$intlon+0.75),
                                    (inDF$intlon+0.25)))
  
  inDF$intlat <- round(inDF[, "LAT"], 0)
  inDF$declat <- inDF[,"LAT"] - inDF$intlat
  inDF$u <- inDF$intlat + 0.25
  inDF$d <- inDF$intlat + 0.75
  inDF[,"LAT_CRU"] <- ifelse(inDF[,"LAT"] < inDF[,"u"], inDF$intlat-0.25,
                             ifelse(inDF[, "LAT"] >= inDF[,"d"], (inDF$intlat+0.75),
                                    (inDF$intlat+0.25)))
  inDF$u <-NULL
  inDF$d <-NULL
  inDF$intlon <- NULL
  inDF$intlat <- NULL
  inDF$declon <- NULL
  inDF$declat <- NULL
  
  inDF <- inDF[c("LON_CRU", "LAT_CRU", "LON", "LAT",
                 "PLT_CN", "CN", "PREV_TRE_CN",
                 "INVYR", "SPCD", "SPGRPCD",
                 "DIA", "DIAHTCD", "HT", "HTCD",
                 "ACTUALHT", "STOCKING", "BHAGE",
                 "TOTAGE", "PREVDIA", "DRYBIO_AG",
                 "TPA_UNADJ")]
  
  return(inDF)
}

####################################################################################
## USDA FIA dataset processing
## Not used in main program, just for information purpose
fia_st_all <- function(cor, fid) {
  ## Function purpose:
  ## Compute plot averages of self-thinning exponents
  ## using all data available
  
  ## library
  #require(raster)
  #require(sm)
  #require(fields)
  #require(spatstat)
  #require(lattice)
  #require(gridExtra)
  
  ## add lon lat onto fid dataframe
  newcor <- data.table(cor$CN, cor$LAT, cor$LON)
  colnames(newcor)<-c("PLT_CN", "LAT", "LON")
  newfid <- merge(fid, newcor, by=c("PLT_CN"))
  
  # taking plot averages
  DF1 <- aggregate(DRYBIO_AG ~ PLT_CN, newfid, mean, na.rm=T)
  DF2 <- aggregate(DRYBIO_BG ~ PLT_CN, newfid, mean, na.rm=T)
  DF3 <- aggregate(DIA ~ PLT_CN, newfid, mean, na.rm=T)
  DF4 <- aggregate(STOCKING ~ PLT_CN, newfid, mean, na.rm=T)
  DF5 <- aggregate(PREVDIA ~ PLT_CN, newfid, mean, na.rm=T)
  DF6 <- aggregate(TPA_UNADJ ~ PLT_CN, newfid, mean, na.rm=T)
  DF7 <- aggregate(CARBON_AG ~ PLT_CN, newfid, mean, na.rm=T)
  DF8 <- aggregate(CARBON_BG ~ PLT_CN, newfid, mean, na.rm=T)

  # merging onto original coordinates, based on PLT_CN indexing
  newDF <- merge(DF1, newcor, by=c("PLT_CN"))
  newDF <- merge(newDF, DF2, by=c("PLT_CN"))
  newDF <- merge(newDF, DF3, by=c("PLT_CN"))
  newDF <- merge(newDF, DF4, by=c("PLT_CN"))
  newDF <- merge(newDF, DF5, by=c("PLT_CN"))
  newDF <- merge(newDF, DF6, by=c("PLT_CN"))
  newDF <- merge(newDF, DF7, by=c("PLT_CN"))
  newDF <- merge(newDF, DF8, by=c("PLT_CN"))
  
  # remove NAs
  newDF <- newDF[complete.cases(newDF),]
  
  # exponential function
  test <- lm(log(newDF$DIA)~log(newDF$TPA_UNADJ)) # best model so far
  summary(test)
}  
  
####################################################################################
## USDA FIA dataset processing
fia_st_species <- function(fid) {
  ## Function purpose:
  ## Compute spgrpcd of self-thinning exponents, intercept, and R-square values
  
  # generate species group list
  spgrpcd.list <- unique(fid$SPGRPCD)
  spgrpcd.list <- spgrpcd.list[!is.na(spgrpcd.list)]
  
  # create output dataframe
  ll <- length(spgrpcd.list)
  outDF <- as.data.frame(matrix(ncol=7, nrow=ll))
  colnames(outDF) <- c("spgrpcd", "T1_exp", "T1_int", "T1_rsq",
                       "T2_exp", "T2_int", "T2_rsq")
  outDF$spgrpcd <- spgrpcd.list
  
  # species group level exponential function
  for (i in spgrpcd.list) {
    # subsetting based on species group indexing
    subDF <- subset(fid, SPGRPCD == i)
    
    # keep complete cases only
    subDF <- subDF[complete.cases(subDF$DRYBIO_AG),]
    subDF <- subDF[complete.cases(subDF$DRYBIO_BG),]
    subDF <- subDF[complete.cases(subDF$DIA),]
    subDF <- subDF[complete.cases(subDF$TPA_UNADJ),]
    subDF <- subset(subDF, TPA_UNADJ > 0)
    
    # test 1: drybio_ag + drybio_bg ~ tpa_unadj
    test1 <- lm(log(subDF$DRYBIO_AG + subDF$DRYBIO_BG)~log(subDF$TPA_UNADJ))
    outDF[outDF$spgrpcd == i, "T1_exp"] <- summary(test1)$coefficients[[2]]
    outDF[outDF$spgrpcd == i, "T1_int"] <- summary(test1)$coefficients[[1]]
    outDF[outDF$spgrpcd == i, "T1_rsq"] <- summary(test1)$r.squared
    
    test2 <- lm(log(subDF$DIA)~log(subDF$TPA_UNADJ))
    outDF[outDF$spgrpcd == i, "T2_exp"] <- summary(test2)$coefficients[[2]]
    outDF[outDF$spgrpcd == i, "T2_int"] <- summary(test2)$coefficients[[1]]
    outDF[outDF$spgrpcd == i, "T2_rsq"] <- summary(test2)$r.squared
  }

  return(outDF)
}  
  
  
####################################################################################
## USDA FIA dataset processing
## Not used in main program, just for information purpose
fia_st_plot <- function(cor, fid) {
  ## Function purpose:
  ## Compute plot averages of self-thinning exponents
  ## using all data available
  
  ## library
  #require(raster)
  #require(sm)
  #require(fields)
  #require(spatstat)
  #require(lattice)
  #require(gridExtra)
  
  ## add lon lat onto fid dataframe
  newcor <- data.table(cor$CN, cor$LAT, cor$LON)
  colnames(newcor)<-c("PLT_CN", "LAT", "LON")
  newfid <- merge(fid, newcor, by=c("PLT_CN"))
  
  ## reduce data size by removing NAs and 0s
  subDF <- subset(newfid, STATUSCD == 1)
  subDF <- data.frame(subDF$PLT_CN, subDF$DIA, subDF$TPA_UNADJ)
  colnames(subDF) <- c("PLT_CN", "DIA", "TPA_UNADJ")
  subDF <- subDF[complete.cases(subDF$DIA),]
  subDF <- subDF[complete.cases(subDF$TPA_UNADJ),]
  subDF <- subset(subDF, TPA_UNADJ > 0)
  subDF <- subset(subDF, DIA > 0)
  
  ## checking number of trees within each plot
  freqDF <- as.data.frame(table(subDF$PLT_CN))
  freqDF <- subset(freqDF, Freq >= 30)
  
  ## create cn list
  cn.list <- unique(freqDF$Var1)
  ll <- length(cn.list)
  
  ## create output dataframe
  outDF <- as.data.frame(matrix(ncol=5, nrow=ll))
  colnames(outDF) <- c("PLT_CN", "exp", "int", "rsq", "num")
  outDF$PLT_CN <- cn.list
  
  # exponential curve fitting
  for (i in cn.list) {
    new <- subset(subDF, PLT_CN == i)
    test1 <- lm(log(new$DIA)~log(new$TPA_UNADJ))
    outDF[outDF$PLT_CN == i, "exp"] <- summary(test1)$coefficients[[2]]
    outDF[outDF$PLT_CN == i, "int"] <- summary(test1)$coefficients[[1]]
    outDF[outDF$PLT_CN == i, "rsq"] <- summary(test1)$r.squared
    outDF[outDF$PLT_CN == i, "num"] <- length(new$PLT_CN)
  }
  
  # add coordinates
  final <- merge(outDF, newcor, by=c("PLT_CN"))
  
  return(final)
}

####################################################################################
## Project fia exponent dataset onto cru grids
exp_to_cru <- function(inDF) {
  inDF$intlon <- round(inDF[,"LON"], 0)
  inDF$declon <- inDF[,"LON"] - inDF$intlon
  inDF$u <- inDF$intlon + 0.25
  inDF$d <- inDF$intlon + 0.75
  inDF[,"LON_CRU"] <- ifelse(inDF[,"LON"] < inDF[,"u"], inDF$intlon-0.25,
                             ifelse(inDF[, "LON"] >= inDF[,"d"], (inDF$intlon+0.75),
                                    (inDF$intlon+0.25)))
  
  inDF$intlat <- round(inDF[, "LAT"], 0)
  inDF$declat <- inDF[,"LAT"] - inDF$intlat
  inDF$u <- inDF$intlat + 0.25
  inDF$d <- inDF$intlat + 0.75
  inDF[,"LAT_CRU"] <- ifelse(inDF[,"LAT"] < inDF[,"u"], inDF$intlat-0.25,
                             ifelse(inDF[, "LAT"] >= inDF[,"d"], (inDF$intlat+0.75),
                                    (inDF$intlat+0.25)))
  inDF$u <-NULL
  inDF$d <-NULL
  inDF$intlon <- NULL
  inDF$intlat <- NULL
  inDF$declon <- NULL
  inDF$declat <- NULL
  
  return(inDF)
}

####################################################################################
## Aggregate fia exponent data onto CRU grids

aggreCRU <- function(inDF, cru) {
  
  # subset plot r square >= 0.3
  subDF <- subset(inDF, rsq >=0.3)
  subDF <- subset(subDF, num >= 50)
  
  # checking
  #newDF <- subDF[order(subDF$exp),]
  #head(newDF)
  
  ## Grid averages of tree statistical parameters
  expDF <- aggregate(subDF[,"exp", drop=F], subDF[,8:9], mean, na.rm=T)
  intDF <- aggregate(subDF[,"int", drop=F], subDF[,8:9], mean, na.rm=T)
  rsqDF <- aggregate(subDF[,"rsq", drop=F], subDF[,8:9], mean, na.rm=T)
  
  newDF <- data.frame(expDF, intDF$int, rsqDF$rsq)
  colnames(newDF) <- c("LON", "LAT", "exp", "int", "rsq")
  
  for (i in unique(newDF$LON)) {
    for (j in unique(newDF$LAT)) {
      
      grid.check <- length(cru[cru$lon == i & cru$lat == j, "temp"])
      
      newDF[newDF$LON == i & newDF$LAT == j, "CRU_Site"] <- ifelse(grid.check > 0, 
                                                                   cru[cru$lon == i & cru$lat == j, "CRU_Site"],
                                                                   NA)
      newDF[newDF$LON == i & newDF$LAT == j, "tempP"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "tempP"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "tempC"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "tempC"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "tempM"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "tempM"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "BIOME"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "BIOME"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "precP"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "precP"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "precC"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "precC"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "precM"] <- ifelse(grid.check > 0, 
                                                                cru[cru$lon == i & cru$lat == j, "precM"],
                                                                NA)
      newDF[newDF$LON == i & newDF$LAT == j, "temp"] <- ifelse(grid.check > 0, 
                                                               cru[cru$lon == i & cru$lat == j, "temp"],
                                                               NA)
      newDF[newDF$LON == i & newDF$LAT == j, "prec_sum"] <- ifelse(grid.check > 0, 
                                                                   cru[cru$lon == i & cru$lat == j, "prec_sum"],
                                                                   NA)
      
      newDF[newDF$LON == i & newDF$LAT == j, "PC1"] <- ifelse(grid.check > 0, 
                                                                   cru[cru$lon == i & cru$lat == j, "PC1"],
                                                                   NA)
      
      newDF[newDF$LON == i & newDF$LAT == j, "PC2"] <- ifelse(grid.check > 0, 
                                                                   cru[cru$lon == i & cru$lat == j, "PC2"],
                                                                   NA)
    } # j
  }   # i
  
  return(newDF)
}


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


####################################################################################
ozflux_extraction <- function(cru) {
  
  # read table
  ozsite <- read.table("/Users/mingkaijiang/Documents/PostDoc/OzFlux/Site_coordinates.csv",
                       header=T,sep=",")
  
  # Project ozflux grids to CRU grids
  ozcru <- oz_to_cru(ozsite)
  cru.name <- names(cru)
  
  # Extract CRU climate information
  for (i in unique(ozcru$LON_CRU)) {
    for (j in unique(ozcru$LAT_CRU)) {
      
      # checking data point presence in cru dataframe
      grid.check <- length(cru[cru$lon == i & cru$lat == j, "temp"])
      
      # assign values onto all columns
      for (n in 1: length(cru.name)) {
        ozcru[ozcru$LON_CRU == i & ozcru$LAT_CRU == j, cru.name[n]] <- ifelse(grid.check > 0, 
                                                                              cru[cru$lon == i & cru$lat == j, cru.name[n]],
                                                                              NA)
      } # n
    } # j
  }  # i
  
  return(ozcru)
  
}

####################################################################################
## Plot oz flux climate data
OzPlot <- function(inDF) {
  
  # color list NOTE: not to be confused with col.list in Main program
  col.list2 <- c("red", "green")
  
  # setting graphics
  par(oma=c(1,1,2,2),
      mar=c(2,10,2,2))
  
  
  # order with mean annual temperature
  inDF <- inDF[order(inDF$temp),]
  
  # Temperature PCM
  
  # create a subset of data table
  plotDF2 <- t(data.frame(inDF$SiteName, inDF$tempC, inDF$tempM))
  colnames(plotDF2) <- plotDF2[1,]
  plotDF2 <- plotDF2[-1,]
  rownames(plotDF2) <- c("C", "M")
  
  # bar plot
  barplot(plotDF2, horiz=T, col=col.list2,las=2,xlim=c(0,1.4),
          main = "Temperature predictability")
  
  legend("bottomright", c("C", "M"), bty="n", fill=col.list2)
  
  # temperature labels
  text(1.1, 24.8, inDF[21, "temp"])
  text(1.05, 23.6, inDF[20, "temp"])
  text(1.05, 22.4, inDF[19, "temp"])
  text(1.1, 21.2, inDF[18, "temp"])
  text(1.05, 20.0, inDF[17, "temp"])
  text(1.05, 18.7, inDF[16, "temp"])
  text(1.05, 17.6, inDF[15, "temp"])
  text(0.98, 16.4, inDF[14, "temp"])
  text(0.98, 15.2, inDF[13, "temp"])
  text(0.95, 14.0, inDF[12, "temp"])
  text(0.95, 12.7, inDF[11, "temp"])
  text(0.85, 11.5, inDF[10, "temp"])
  text(0.85, 10.3, inDF[9, "temp"])
  text(0.85, 9.0, inDF[8, "temp"])
  text(0.85, 7.8, inDF[7, "temp"])
  text(0.85, 6.6, inDF[6, "temp"])
  text(0.85, 5.4, inDF[5, "temp"])
  text(0.85, 4.2, inDF[4, "temp"])
  text(0.85, 3, inDF[3, "temp"])
  text(0.85, 1.8, inDF[2, "temp"])
  text(0.85, 0.6, inDF[1, "temp"])
  
  # Precipitation PCM
  # order with precipitation totals
  inDF <- inDF[order(inDF$prec_sum),]
  # create a subset of data table
  plotDF2 <- t(data.frame(inDF$SiteName, inDF$precC, inDF$precM))
  colnames(plotDF2) <- plotDF2[1,]
  plotDF2 <- plotDF2[-1,]
  rownames(plotDF2) <- c("C", "M")
  
  # bar plot
  barplot(plotDF2, horiz=T, col=col.list2,las=2, xlim = c(0,0.8),
          main = "Precipitation predictability")
  
  legend("bottomright", c("C", "M"), bty="n", fill=col.list2)
  
  # precipitation labels
  text(0.45, 24.8, round(inDF[21, "prec_sum"],0))
  text(0.45, 23.6, round(inDF[20, "prec_sum"],0))
  text(0.54, 22.4, round(inDF[19, "prec_sum"],0))
  text(0.54, 21.2, round(inDF[18, "prec_sum"],0))
  text(0.54, 20.0, round(inDF[17, "prec_sum"],0))
  text(0.54, 18.7, round(inDF[16, "prec_sum"],0))
  text(0.5, 17.6, round(inDF[15, "prec_sum"],0))
  text(0.5, 16.4, round(inDF[14, "prec_sum"],0))
  text(0.5, 15.2, round(inDF[13, "prec_sum"],0))
  text(0.5, 14.0, round(inDF[12, "prec_sum"],0))
  text(0.54, 12.7, round(inDF[11, "prec_sum"],0))
  text(0.5, 11.5, round(inDF[10, "prec_sum"],0))
  text(0.6, 10.3, round(inDF[9, "prec_sum"],0))
  text(0.5, 9.0, round(inDF[8, "prec_sum"],0))
  text(0.5, 7.8, round(inDF[7, "prec_sum"],0))
  text(0.5, 6.6, round(inDF[6, "prec_sum"],0))
  text(0.45, 5.4, round(inDF[5, "prec_sum"],0))
  text(0.45, 4.2, round(inDF[4, "prec_sum"],0))
  text(0.4, 3, round(inDF[3, "prec_sum"],0))
  text(0.4, 1.8, round(inDF[2, "prec_sum"],0))
  text(0.4, 0.6, round(inDF[1, "prec_sum"],0))
  
  par(opar)
  
  
}

####################################################################################
# Plot Australia maps
Aus_Plot <- function(cru) {
  
  ## Plot the following maps:
  ## 1. global temperature profile map
  ## 2. global temperature predictability map
  ## 3. global temperature constancy map
  ## 4. global temperautre contigency map
  ## 5. temperature vs. temperature predictability map, grouped by IE factor
  ## 6. Violin plot of the latitudinal patterns of Ie factor
  
  ##### temperature 
  
  # plot temperature mean map
  par(oma=c(1,1,2,2),
      mar=c(2,2,2,5))
  with(cru, quilt.plot(lon, lat, templab, nx=720, ny=280, 
                        main="Annual mean temperature", xlim=c(110,160), ylim=c(-45,-5),
                        add.legend=F))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = temp.lab, title = expression(paste("Temperature [",degree,"C]")),
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
  # plot temperature predictability map
  with(cru, quilt.plot(lon, lat, tempP, nx=720, ny=280,  nlevel=12,
                       xlim=c(110,160), ylim=c(-45,-5),
                        main="Temperature predictability", add.legend=T))
  
  # plot temperature constancy map
  with(cru, quilt.plot(lon, lat, tempC, nx=720, ny=280, nlevel=12,
                       xlim=c(110,160), ylim=c(-45,-5),
                        main="Temperature constancy", add.legend=T))
  
  # plot temperature contingency map
  with(cru, quilt.plot(lon, lat, tempM, nx=720, ny=280,  nlevel=12,
                       xlim=c(110,160), ylim=c(-45,-5),
                        main="Temperature contingency", add.legend=T))
  
  ##### precipitation
  
  # plot precipitation map
  par(oma=c(1,1,2,2),
      mar=c(2,2,2,5))
  with(cru, quilt.plot(lon, lat, preclab, nx=720, ny=280, 
                       xlim=c(110,160), ylim=c(-45,-5),
                        main="Annual sum precipitation", add.legend=F))
  par(fig = c(0,1,0,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("right", legend = prec.lab, title = "Precipitation [mm]",
         fill=tim.colors(12), cex=1, bty="n")
  par(opar)
  
  # plot precipitation predictability map
  with(cru, quilt.plot(lon, lat, precP, nx=720, ny=280,  nlevel=12,
                       xlim=c(110,160), ylim=c(-45,-5),
                        main="Precipitation predictability", add.legend=T))
  
  # plot Precipitation constancy map
  with(cru, quilt.plot(lon, lat, precC, nx=720, ny=280,  nlevel=12,
                        xlim=c(110,160), ylim=c(-45,-5),
                        main="Precipitation constancy", add.legend=T))
  
  # plot Precipitation contingency map
  with(cru, quilt.plot(lon, lat, precM, nx=720, ny=280,  nlevel=12,
                        xlim=c(110,160), ylim=c(-45,-5),
                        main="Precipitation contingency", add.legend=T))
  
}

###################################################################################
## Save ozflux coordinates onto CRU coordinate system
oz_to_cru <- function(inDF) {
  inDF$intlon <- round(inDF[,"Lon"], 0)
  inDF$declon <- inDF[,"Lon"] - inDF$intlon
  inDF$u <- inDF$intlon + 0.25
  inDF$d <- inDF$intlon + 0.75
  inDF[,"LON_CRU"] <- ifelse(inDF[,"Lon"] < inDF[,"u"], inDF$intlon-0.25,
                             ifelse(inDF[, "Lon"] >= inDF[,"d"], (inDF$intlon+0.75),
                                    (inDF$intlon+0.25)))
  
  inDF$intlat <- round(inDF[, "Lat"], 0)
  inDF$declat <- inDF[,"Lat"] - inDF$intlat
  inDF$u <- inDF$intlat + 0.25
  inDF$d <- inDF$intlat + 0.75
  inDF[,"LAT_CRU"] <- ifelse(inDF[,"Lat"] < inDF[,"u"], inDF$intlat-0.25,
                             ifelse(inDF[, "Lat"] >= inDF[,"d"], (inDF$intlat+0.75),
                                    (inDF$intlat+0.25)))
  inDF$u <-NULL
  inDF$d <-NULL
  inDF$intlon <- NULL
  inDF$intlat <- NULL
  inDF$declon <- NULL
  inDF$declat <- NULL
  
  return(inDF)
}

####################################################################################
## plotting animated density plot for climate
AnimatedDensityPlot <- function(inDF) {
  ## Plotting biome-specific density graphs for:
  ## 1. temperature vs. precipitation
  ## 2. temperature predictability vs. precipitation predictability
  
  # library
  require(vegan)
  require(ks)
  
  # prepare dataframe
  plotDF2 <- subset(inDF, BIOME > 0)
  plotDF2 <- subset(plotDF2, BIOME < 98)

  # set frames
  frames = 14
  
  ## Plot temp vs. prec for each biome
  for (i in 1:frames) {
    
    # creating a name for each plot file with leading zeros
    if (i < 10) {name = paste('000',i,'plot.png',sep='')}
    if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
    
    png(name, width=960, height=480)
    
    # Set output graph structure
    set.panel()
    par(oma=c(2,4,2,2),
        mar=c(5.1,5.1,4.1,1.2),
        mgp = c(3, 1, 0))
    layout(matrix(c(1,2),1,2))

    # subsetting dataframe
    DF <- data.frame(plotDF2[plotDF2$BIOME == i, "temp"],
                     plotDF2[plotDF2$BIOME == i, "prec_sum"])
    # kernel density estimation
    H <- Hpi(x=DF)      # optimal bandwidth estimation
    est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    
    # set contour probabilities for drawing contour levels
    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
         ylab="Precipitation", xlab="Temperature", main=biome[i],
         ylim=c(0,8000), xlim=c(-30,40),las=1, cex.lab=2, cex.main=2) 
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    
    # subsetting dataframe
    DF <- data.frame(plotDF2[plotDF2$BIOME == i, "tempP"],
                     plotDF2[plotDF2$BIOME == i, "precP"])
    # kernel density estimation
    H <- Hpi(x=DF)      # optimal bandwidth estimation
    est<- kde(x=DF, H=H, compute.cont=TRUE)     # kernel density estimation
    
    # set contour probabilities for drawing contour levels
    cl<-contourLevels(est, prob=c(0.5, 0.05, 0.001), approx=TRUE)
    
    plot(est, cont=seq(1,100,by=1), display="filled.contour2", add=FALSE, 
         ylab="Precipitation predictability", xlab="Temperature predictability", main=biome[i],
         cex.lab=2, cex.main=2, ylim=c(0,1), xlim=c(0,1),las=1) 
    plot(est,abs.cont=cl[1], labels=c(0.5),labcex=0.75, add=TRUE, lwd=0.75, col="grey30")
    plot(est,abs.cont=cl[2], labels=c(0.95),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    plot(est,abs.cont=cl[3], labels=c(0.99),labcex=0.75, add=TRUE, lwd=0.5, col="grey60")
    
    dev.off()
    
    par(opar)
  }
}


####################################################################################
## plotting animated density plot for climate
Animated_radar <- function(inDF) {
  ## Plotting biome-specific density graphs for:
  ## 1. temperature vs. precipitation
  ## 2. temperature predictability vs. precipitation predictability
  
  # library
  require(fmsb)
  
  # prepare dataframe
  col.list <- rainbow(14)
  newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                      inDF$tempP_mean,inDF$precP_mean)
  names(newDF) <- c("temp", "prec", "temp P", "prec P")
  rownames(newDF) <- biome
  
  # set frames
  frames = 14
  
  ## Plot temp vs. prec for each biome
  for (i in 1:frames) {
    
    # creating a name for each plot file with leading zeros
    if (i < 10) {name = paste('000',i,'plot.png',sep='')}
    if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
    
    png(name, width=960, height=480)
    plotDF <- rbind(c(30, 2500, 1, 1), c(-20, 0, 0, 0), newDF[i,])
    radarchart(plotDF, pcol=col.list[i], 
               vlcex=2, title=biome[i],cex.main=2,
               pfcol=adjustcolor(col.list[i],0.2))
   # title(biome[i], cex.lab=2)
      
    dev.off()
      
    par(opar)
  }

}

####################################################################################
## Extract Emerald and Gingin sites time-series data
TwoSites_summary <- function(oz) {
  
  # library
  require(matrixStats)
  require(reshape2)
  require(lattice)
  
  # read in raw climate data
  temp <- read.table(paste(getwd(), "/temp_DF_processed.csv", sep=""),
                     header=T,sep=",")
  prec <- read.table(paste(getwd(), "/pre_DF_processed.csv", sep=""),
                     header=T,sep=",")
  
  # prepare the site coordinates
  Ginlat <- oz[oz$SiteName == "Gingin", "LAT_CRU"]
  Ginlon <- oz[oz$SiteName == "Gingin", "LON_CRU"]
    
  Emelat <- oz[oz$SiteName == "Emerald", "LAT_CRU"]
  Emelon <- oz[oz$SiteName == "Emerald", "LON_CRU"]
  
  # subset time series 
  GinT <- subset(temp, lon == Ginlon & lat == Ginlat)
  GinP <- subset(prec, lon == Ginlon & lat == Ginlat)
  EmeT <- subset(temp, lon == Emelon & lat == Emelat)
  EmeP <- subset(prec, lon == Emelon & lat == Emelat)
  
  # Calculate intra- and inter-annual coef of variations
  # intraannual
  GinT_colmean <- colMeans(GinT[,5:16])
  GinT_sd <- sd(GinT_colmean)
  GinT_intra <- mean(GinT_sd/GinT_colmean)
  
  EmeT_colmean <- colMeans(EmeT[,5:16])
  EmeT_sd <- sd(EmeT_colmean)
  EmeT_intra <- mean(EmeT_sd/EmeT_colmean)
  
  GinP_colmean <- colMeans(GinP[,5:16])
  GinP_sd <- sd(GinP_colmean)
  GinP_intra <- mean(GinP_sd/GinP_colmean)
  
  EmeP_colmean <- colMeans(EmeP[,5:16])
  EmeP_sd <- sd(EmeP_colmean)
  EmeP_intra <- mean(EmeP_sd/EmeP_colmean)
  
  # interannual
  GinT_rowmean <- rowMeans(GinT[,5:16])
  GinT_sd <- sd(GinT_rowmean)
  GinT_inter <- mean(GinT_sd/GinT_rowmean)
  
  EmeT_rowmean <- rowMeans(EmeT[,5:16])
  EmeT_sd <- sd(EmeT_rowmean)
  EmeT_inter <- mean(EmeT_sd/EmeT_rowmean)
  
  GinP_rowmean <- rowMeans(GinP[,5:16])
  GinP_sd <- sd(GinP_rowmean)
  GinP_inter <- mean(GinP_sd/GinP_rowmean)
  
  EmeP_rowmean <- rowMeans(EmeP[,5:16])
  EmeP_sd <- sd(EmeP_rowmean)
  EmeP_inter <- mean(EmeP_sd/EmeP_rowmean)
  
  # Prepare output table
  outDF <- matrix(ncol=2, nrow=4)
  colnames(outDF) <- c("Gingin", "Emerald")
  rownames(outDF) <- c("T_inter", "T_intra", "P_inter", "P_intra")
  
  outDF["T_inter", "Gingin"] <- GinT_inter
  outDF["T_intra", "Gingin"] <- GinT_intra
  outDF["P_inter", "Gingin"] <- GinP_inter
  outDF["P_intra", "Gingin"] <- GinP_intra
  outDF["T_inter", "Emerald"] <- EmeT_inter
  outDF["T_intra", "Emerald"] <- EmeT_intra
  outDF["P_inter", "Emerald"] <- EmeP_inter
  outDF["P_intra", "Emerald"] <- EmeP_intra
  
  return(outDF)
  
}

####################################################################################
## Extract Emerald and Gingin sites time-series data
TwoSites_timeseries <- function(oz) {
  
  # library
  require(matrixStats)
  require(reshape2)
  require(lattice)
  require(gridExtra)
  
  # read in raw climate data
  temp <- read.table(paste(getwd(), "/temp_DF_processed.csv", sep=""),
                     header=T,sep=",")
  prec <- read.table(paste(getwd(), "/pre_DF_processed.csv", sep=""),
                     header=T,sep=",")
  
  # prepare the site coordinates
  Ginlat <- oz[oz$SiteName == "Gingin", "LAT_CRU"]
  Ginlon <- oz[oz$SiteName == "Gingin", "LON_CRU"]
  
  Emelat <- oz[oz$SiteName == "Emerald", "LAT_CRU"]
  Emelon <- oz[oz$SiteName == "Emerald", "LON_CRU"]
  
  # subset time series 
  GinT <- subset(temp, lon == Ginlon & lat == Ginlat)
  GinP <- subset(prec, lon == Ginlon & lat == Ginlat)
  EmeT <- subset(temp, lon == Emelon & lat == Emelat)
  EmeP <- subset(prec, lon == Emelon & lat == Emelat)

  # Prepare dataframe to contain time series data
  GinPlot <- matrix(ncol=4,nrow=112*12)
  GinPlot <- as.data.frame(GinPlot)
  colnames(GinPlot) <- c("Year", "Month", "Temp", "Prec")
  GinPlot$Year <- seq(1901, 2012, by = 1)
  GinPlot <- GinPlot[order(GinPlot$Year),]
  GinPlot$Month <- seq(1, 12, by=1)
  
  GinPlot$Date <- paste(GinPlot$Year, GinPlot$Month, sep="-")
  GinPlot$Date <- as.yearmon(GinPlot$Date)
  
  EmePlot <- GinPlot
  
  # Assign values onto the dataframe Gingin
  for (i in 1901:2012) {
    subT <- subset(GinT, year == i)
    subP <- subset(GinP, year == i)
    
    transT <- t(subT[,5:16])
    transP <- t(subP[,5:16])
    
    GinPlot[GinPlot$Year == i, "Temp"] <- transT
    GinPlot[GinPlot$Year == i, "Prec"] <- transP
  }
  
  # Assign values onto the dataframe Emerald
  for (i in 1901:2012) {
    subT <- subset(EmeT, year == i)
    subP <- subset(EmeP, year == i)
    
    transT <- t(subT[,5:16])
    transP <- t(subP[,5:16])
    
    EmePlot[EmePlot$Year == i, "Temp"] <- transT
    EmePlot[EmePlot$Year == i, "Prec"] <- transP
  }
  
  # interannual plot DF
  GinterDF <- as.data.frame(matrix(ncol = 5, nrow = 112))
  colnames(GinterDF) <- c("Year", "Temp_mean", "Temp_sd", "Prec_mean", "Prec_sd")
  GinterDF$Year <- c(1901:2012)
  EinterDF <- GinterDF
  GinterDF$Temp_mean <- rowMeans(GinT[,5:16])
  GinterDF$Prec_mean <- rowMeans(GinP[,5:16])
  
  for (i in 1901:2012) {
    GinterDF[GinterDF$Year == i, "Temp_sd"] <- sd(GinT[GinT$year == i,5:16])
    GinterDF[GinterDF$Year == i, "Prec_sd"] <- sd(GinP[GinP$year == i,5:16])
  }
  EinterDF$Temp_mean <- rowMeans(EmeT[,5:16])
  EinterDF$Prec_mean <- rowMeans(EmeP[,5:16])
  
  for (i in 1901:2012) {
    EinterDF[EinterDF$Year == i, "Temp_sd"] <- sd(EmeT[EmeT$year == i,5:16])
    EinterDF[EinterDF$Year == i, "Prec_sd"] <- sd(EmeP[EmeP$year == i,5:16])
  }
  
  # monthly plot DF
  GintraDF <- as.data.frame(matrix(ncol = 5, nrow = 12))
  colnames(GintraDF) <- c("Month", "Temp_mean", "Temp_sd", "Prec_mean", "Prec_sd")
  GintraDF$Month <- c(1:12)
  EintraDF <- GintraDF
  GintraDF$Temp_mean <- colMeans(GinT[,5:16])
  GintraDF$Prec_mean <- colMeans(GinP[,5:16])
  for (i in 1:12) {
    GintraDF[GintraDF$Month == i, "Temp_sd"] <- sd(GinT[, 4+i])
    GintraDF[GintraDF$Month == i, "Prec_sd"] <- sd(GinP[, 4+i])
  }
  EintraDF$Temp_mean <- colMeans(EmeT[,5:16])
  EintraDF$Prec_mean <- colMeans(EmeP[,5:16])
  for (i in 1:12) {
    EintraDF[EintraDF$Month == i, "Temp_sd"] <- sd(EmeT[, 4+i])
    EintraDF[EintraDF$Month == i, "Prec_sd"] <- sd(EmeP[, 4+i])
  }
  
  set.panel()
  par(oma=c(2,4,2,2),
      mar=c(5.1,5.1,4.1,1.2),
      mgp = c(3, 1, 0))
  set.panel(1,2)
  
  # Make plot GinGin
  myDF <- melt(subset(GinPlot, select=c(Date, Temp, Prec)), 
               id.var="Date")
  
  p1 <- xyplot(value~Date|variable, data=myDF, type="l",main="GinGin",
         scales=list(y=list(relation="free")),
         layout=c(1,2))

  # Make plot Emerald
  myDF <- melt(subset(EmePlot, select=c(Date, Temp, Prec)), 
               id.var="Date")
  
  p2 <- xyplot(value~Date|variable, data=myDF, type="l",main="Emerald",
         scales=list(y=list(relation="free")),
         layout=c(1,2))
  
  grid.arrange(p1, p2, ncol=2)
  
  set.panel()
  par(oma=c(2,4,2,2),
      mar=c(5.1,5.1,4.1,1.2),
      mgp = c(3, 1, 0))
  set.panel(2,2)
  
  ## Interannual variability
  
  with(GinterDF, plot(Prec_mean~Year, type="l", 
                      col = "red", ylab = "Precipitation",
                      ylim = c(0, 200), lwd = 4,
                      main = "Gingin interannual variability",
                      cex.lab = 2, cex.main = 2))
  with(GinterDF, polygon(c(Year, rev(Year)), 
          c((Prec_mean+Prec_sd), rev(Prec_mean-Prec_sd)),
          col = adjustcolor("grey", 0.4), border = NA))
  
  with(EinterDF, plot(Prec_mean~Year, type="l", 
                      col = "red", ylab = "Precipitation",
                      ylim = c(0, 200),lwd = 4,
                      main = "Emerald interannual variability",
                      cex.lab = 2, cex.main = 2))
  with(EinterDF, polygon(c(Year, rev(Year)), 
                         c((Prec_mean+Prec_sd), rev(Prec_mean-Prec_sd)),
                         col = adjustcolor("grey", 0.4), border = NA))
  
  ## Intraanual variability
  
  with(GintraDF, plot(Prec_mean~Month, type="l", 
                      col = "red", ylab = "Precipitation",
                      ylim = c(0, 200), lwd = 4,
                      main = "Gingin monthly variability",
                      cex.lab = 2, cex.main = 2))
  with(GintraDF, polygon(c(Month, rev(Month)), 
                         c((Prec_mean+Prec_sd), rev(Prec_mean-Prec_sd)),
                         col = adjustcolor("grey", 0.4), border = NA))
  
  with(EintraDF, plot(Prec_mean~Month, type="l", 
                      col = "red", ylab = "Precipitation",
                      ylim = c(0, 200),lwd = 4,
                      main = "Emerald monthly variability",
                      cex.lab = 2, cex.main = 2))
  with(EintraDF, polygon(c(Month, rev(Month)), 
                         c((Prec_mean+Prec_sd), rev(Prec_mean-Prec_sd)),
                         col = adjustcolor("grey", 0.4), border = NA))
  
}

####################################################################################
Tasmania_process <- function(inDF) {
  
  # Visually checking
  #with(inDF, quilt.plot(lon, lat, temp, 
  #                        xlim=c(140, 150), ylim=c(-45, -40), 
  #                        nx=720, ny=280))
  
  # read in file
  myDF <- subset(inDF, lon <= 150 & lon >= 140 & lat <= -40 & lat >= -45)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF$temp)
  temp.max <- max(myDF$temp)
  subDF.temp <- subset(inDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF$prec_sum)
  prec.max <- max(myDF$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  # looking for similar grids based on additional tempP min and max
  tempP.min <- min(myDF$tempP)
  tempP.max <- max(myDF$tempP)
  subDF.tempP <- subset(subDF.prec, tempP >= tempP.min & tempP <= tempP.max)
  dim(subDF.tempP)
  
  # looking for similar grids based on additional precP min and max
  precP.min <- min(myDF$precP)
  precP.max <- max(myDF$precP)
  subDF.precP <- subset(subDF.tempP, precP >= precP.min & precP <= precP.max)
  dim(subDF.precP)
  
  # Plot the selection in maps
  
  # temperature criteria
  with(subDF.temp, quilt.plot(lon, lat, temp, 
                              main = "Temperature",
                              xlim=c(-180, 180), ylim=c(-60, 80),
                              nx=680, ny=220))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  # precipitation criteria
  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                              main = "Precipitation",
                              xlim=c(-180, 180), ylim=c(-60, 80),
                              nx=680, ny=220))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  # temperature P criteria
  with(subDF.tempP, quilt.plot(lon, lat, tempP,
                               main = "Temperature predictability",
                               xlim=c(-180, 180), ylim=c(-60, 80),
                              nx=680, ny=220))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  # precipitation P criteria
  with(subDF.precP, quilt.plot(lon, lat, precP,
                               main = "Precipitation predictability",
                               xlim=c(-180, 180), ylim=c(-60, 80),
                              nx=680, ny=220))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  # plot temp IE
  with(subDF.precP, quilt.plot(lon, lat, tempIE,col=c("red", "blue"),
                               main = "Temperature Ie",
                               xlim=c(-180, 180), ylim=c(-60, 80),
                               nx=680, ny=220), add.legend=F)
  legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  # plot prec IE
  with(subDF.precP, quilt.plot(lon, lat, precIE,col=c("red", "blue"),
                               main = "Precipitation Ie",
                               xlim=c(-180, 180), ylim=c(-60, 80),
                               nx=680, ny=220), add.legend=F)
  legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"))
  world(add=T, col=adjustcolor("grey", 0.5))
  
  return(subDF.precP)

}

####################################################################################
Tasmania_compare <- function(inDF) {
  
  # read in file
  myDF <- subset(inDF, lon <= 150 & lon >= 140 & lat <= -40 & lat >= -45)
  myDF$lab <- "Tas"
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF$temp)
  temp.max <- max(myDF$temp)
  subDF.temp <- subset(inDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF$prec_sum)
  prec.max <- max(myDF$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
#  # visually check for regions to subset
#  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
#                              main = "Precipitation",
#                              xlim=c(-180, 180), ylim=c(-60, 80),
#                              nx=680, ny=220))
#  world(add=T, col=adjustcolor("grey", 0.5))
  
  # Tasmania
  with(subDF.prec, quilt.plot(lon, lat, tempP, main="Tasmania",
                              xlim=c(140, 150), ylim=c(-45, -40), 
                              nx=700, ny=200))
  world(add=T)
  
  # EAsia
  with(subDF.prec, quilt.plot(lon, lat, tempP, main="East Asia",
                              xlim=c(80, 160), ylim=c(20, 50), 
                              nx=700, ny=200))
  world(add=T)
  
  # EU
  with(subDF.prec, quilt.plot(lon, lat, tempP, main="Europe",
                              xlim=c(-20, 50), ylim=c(30, 70), 
                              nx=700, ny=200))
  world(add=T)
  
  # NZ
  with(subDF.prec, quilt.plot(lon, lat, tempP, main="New Zealand",
                              xlim=c(160, 180), ylim=c(-60, -30), 
                              nx=700, ny=200))
  world(add=T)
  
  # US
  with(subDF.prec, quilt.plot(lon, lat, tempP, main="United States",
                        xlim=c(-120, -50), ylim=c(30, 70), 
                        nx=700, ny=200))
  world(add=T)
  
  # Subsetting different regions
  
  # New Zealand
  NZ <- subset(subDF.prec, lon <= 180 & lon >= 160 & lat <= -30 & lat >= -60)
  NZ$lab <- "NZ"
  # Eastern Asia
  EA <- subset(subDF.prec, lon <= 160 & lon >= 80 & lat <= 50 & lat >= 20)
  EA$lab <- "EAsia"
  # Europe
  EU <- subset(subDF.prec, lon <= 50 & lon >= -20 & lat <= 70 & lat >= 30)
  EU$lab <- "EU"
  # US
  US <- subset(subDF.prec, lon <= -50 & lon >= -120 & lat <= 70 & lat >= 30)
  US$lab <- "US"
  
  finalDF <- rbind(myDF, NZ, EA, EU, US)
  
  # Plotting
  set.panel()
  #par(oma=c(2,2,2,2),
  #    mgp = c(2, 1, 0))
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow","cyan","orange"),
                        cex.lab=1.2, cex.axis=1.2))
  
  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow","cyan","orange"),
                        cex.lab=1.2, cex.axis=1.2))
  
}

####################################################################################
Australia_process <- function(inDF) {
  
  # Visually checking
  #with(inDF, quilt.plot(lon, lat, temp, 
  #                        xlim=c(110, 155), ylim=c(-45, -12), 
  #                        nx=720, ny=280))
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  
  # looking for biome list from Australia 
  biome.list <- unique(myDF$BIOME)
  biome.list <- biome.list[-1]
  
  set.panel()
  par(oma=c(1,1,2,2),
      mar=c(5,4,4,5),
      mgp = c(3, 1, 0))  
  set.panel(3,2)
  
  # look for climate niche for each biome
  for (i in c(4,7,8,12,13)) {
    myDF1 <- subset(myDF, BIOME == i)
    globDF <- subset(inDF, BIOME == i)
    
    # looking for similar grids based on temp min and max
    temp.min <- min(myDF1$temp)
    temp.max <- max(myDF1$temp)
    subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
    dim(subDF.temp)
    
    # looking for similar grids based on additional prec min and max
    prec.min <- min(myDF1$prec_sum)
    prec.max <- max(myDF1$prec_sum)
    subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
    dim(subDF.prec)
    
    # looking for similar grids based on additional tempP min and max
    tempP.min <- min(myDF1$tempP)
    tempP.max <- max(myDF1$tempP)
    subDF.tempP <- subset(subDF.prec, tempP >= tempP.min & tempP <= tempP.max)
    dim(subDF.tempP)
    
    # looking for similar grids based on additional precP min and max
    precP.min <- min(myDF1$precP)
    precP.max <- max(myDF1$precP)
    subDF.precP <- subset(subDF.tempP, precP >= precP.min & precP <= precP.max)
    dim(subDF.precP)
    
    # Plot the selection in maps
    # setting graphics
    
    # temperature criteria
    with(inDF[inDF$BIOME == i, ], quilt.plot(lon, lat, temp, main = biome[i],
                                xlab = "Temperature",cex.lab=2, cex.main=2,
                                xlim=c(-180, 180), ylim=c(-60, 80),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation criteria
    with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                                xlab = "Precipitation",cex.lab=2, cex.main=2,
                                xlim=c(-180, 180), ylim=c(-60, 80),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # temperature P criteria
    with(subDF.tempP, quilt.plot(lon, lat, tempP,cex.lab=2, cex.main=2,
                                 xlab = "Temperature predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation P criteria
    with(subDF.precP, quilt.plot(lon, lat, precP,cex.lab=2, cex.main=2,
                                 xlab = "Precipitation predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # plot temp IE
    with(subDF.precP, quilt.plot(lon, lat, tempIE,col=c("red", "blue"),
                                 xlab = "Temperature Ie",cex.lab=2, cex.main=2,
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=120), add.legend=F)
    legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"),
           cex=2)
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # plot prec IE
    with(subDF.precP, quilt.plot(lon, lat, precIE,col=c("red", "blue"),
                                 xlab = "Precipitation Ie",cex.lab=2, cex.main=2,
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=680, ny=120), add.legend=F)
    legend("bottomleft", c("Ie > 1", "Ie < 1"), fill = c("blue", "red"),
           cex=2)
    world(add=T, col=adjustcolor("grey", 0.5))
    
    
  }
  
  return(subDF.precP)
  
}

####################################################################################
Australia_process_map <- function(inDF) {
  
  # Visually checking
  #with(inDF, quilt.plot(lon, lat, temp, 
  #                        xlim=c(110, 155), ylim=c(-45, -12), 
  #                        nx=720, ny=280))
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  
  # looking for biome list from Australia 
  biome.list <- unique(myDF$BIOME)
  biome.list <- biome.list[-1]
  
  set.panel()
  par(oma=c(1,1,2,2),
      mar=c(5,4,4,5),
      mgp = c(3, 1, 0))  
  set.panel(2,2)
  
  # look for climate niche for each biome
  for (i in c(4,7,8,12,13)) {
    myDF1 <- subset(myDF, BIOME == i)
    globDF <- subset(inDF, BIOME == i)
    
    # looking for similar grids based on temp min and max
    temp.min <- min(myDF1$temp)
    temp.max <- max(myDF1$temp)
    subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
    dim(subDF.temp)
    
    # looking for similar grids based on additional prec min and max
    prec.min <- min(myDF1$prec_sum)
    prec.max <- max(myDF1$prec_sum)
    subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
    dim(subDF.prec)
    
    # Plot the selection in maps
    
    # temperature criteria
    with(subDF.prec, quilt.plot(lon, lat, temp, main = biome[i],
                                             xlab = "Temperature",cex.lab=2, cex.main=2,
                                             xlim=c(-180, 180), ylim=c(-60, 80),
                                             nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation criteria
    with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
                                xlab = "Precipitation",cex.lab=2, cex.main=2,
                                xlim=c(-180, 180), ylim=c(-60, 80),
                                nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # temperature P criteria
    with(subDF.prec, quilt.plot(lon, lat, tempP,cex.lab=2, cex.main=2,
                                 xlab = "Temperature predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    # precipitation P criteria
    with(subDF.prec, quilt.plot(lon, lat, precP,cex.lab=2, cex.main=2,
                                 xlab = "Precipitation predictability",
                                 xlim=c(-180, 180), ylim=c(-60, 80),
                                 nx=400, ny=120))
    world(add=T, col=adjustcolor("grey", 0.5))
  }
  
  return(subDF.prec)
  
}

####################################################################################
Australia_compare <- function(inDF) {
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 4)
  globDF <- subset(inDF, BIOME == 4)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
#  # visually check
#  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
#                              xlab = "Precipitation",cex.lab=2, cex.main=2,
#                              xlim=c(-180, 180), ylim=c(-60, 80),
#                              nx=400, ny=120))
#  world(add=T, col=adjustcolor("grey", 0.5))
    
  # map plot
  # Australia
  with(subDF.prec, quilt.plot(lon, lat, precP, 
                              main = "Australia",
                              xlim=c(110, 155), ylim=c(-45, -12),
                              nx=400, ny=120))
  world(add=T, col=adjustcolor("black", 1))
  
  # China
  with(subDF.prec, quilt.plot(lon, lat, precP, 
                              main = "China",
                              xlim=c(80, 160), ylim=c(20, 50),
                              nx=400, ny=120))
  world(add=T, col=adjustcolor("black", 1))
  
  # EU
  with(subDF.prec, quilt.plot(lon, lat, precP, 
                              main = "Europe",
                              xlim=c(-20, 50), ylim=c(30, 70),
                              nx=400, ny=120))
  world(add=T, col=adjustcolor("black", 1))
  
  # US
  with(subDF.prec, quilt.plot(lon, lat, precP, 
                              main = "United States",
                              xlim=c(-120, -50), ylim=c(30, 70),
                              nx=400, ny=120))
  world(add=T, col=adjustcolor("black", 1))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  AU$lab <- "AU"
  # China
  CH <- subset(subDF.prec, lon <= 160 & lon >= 80 & lat <= 50 & lat >= 20)
  CH$lab <- "CH"
  # Europe
  EU <- subset(subDF.prec, lon <= 50 & lon >= -20 & lat <= 70 & lat >= 30)
  EU$lab <- "EU"
  # US
  US <- subset(subDF.prec, lon <= -50 & lon >= -120 & lat <= 70 & lat >= 30)
  US$lab <- "US"
  
  finalDF <- rbind(AU, CH, EU, US)
  
  # Plotting
  set.panel()
  #par(oma=c(2,2,2,2),
  #    mgp = c(2, 1, 0))
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  
  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
}

####################################################################################
Australia_compare_4 <- function(inDF) {
  
  #library
  require(agricolae)
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 4)
  globDF <- subset(inDF, BIOME == 4)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  #  # visually check
  #  with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
  #                              xlab = "Precipitation",cex.lab=2, cex.main=2,
  #                              xlim=c(-180, 180), ylim=c(-60, 80),
  #                              nx=400, ny=120))
  #  world(add=T, col=adjustcolor("grey", 0.5))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  AU$lab <- "AU"
  # China
  CH <- subset(subDF.prec, lon <= 160 & lon >= 80 & lat <= 50 & lat >= 20)
  CH$lab <- "CH"
  # Europe
  EU <- subset(subDF.prec, lon <= 50 & lon >= -20 & lat <= 70 & lat >= 30)
  EU$lab <- "EU"
  # US
  US <- subset(subDF.prec, lon <= -50 & lon >= -120 & lat <= 70 & lat >= 30)
  US$lab <- "US"
  
  finalDF <- rbind(AU, CH, EU, US)
  finalDF$lab <- factor(finalDF$lab)
  
  # statistical test
  #mod1 <- lm(tempP~lab, data=finalDF)
  #summary(mod1)
  #anova(mod1)
  #confint(mod1)
  aov.out1 <- aov(tempP~lab, data=finalDF)
  out1 <- HSD.test(aov.out1, "lab")
  
  aov.out2 <- aov(precP~lab, data=finalDF)
  out2 <- HSD.test(aov.out2, "lab")
  
  aov.out3 <- aov(precC~lab, data=finalDF)
  out3 <- HSD.test(aov.out3, "lab")
  
  aov.out4 <- aov(precM~lab, data=finalDF)
  out4 <- HSD.test(aov.out4, "lab")
  
  # Plotting
  set.panel()
  set.panel(2,2)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
  mtext("a", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.62, line = -2, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.85, line = -2.2, cex = 1.5, ft=2)
  
  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
  mtext("d", side = 3, adj = 0.85, line = -1.2, cex = 1.5, ft=2)
  
  # Precipitation constancy
  with(finalDF, boxplot(precC ~ lab, xlab = "Regions", 
                        ylab = "Precipitation constancy",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
  mtext("d", side = 3, adj = 0.88, line = -2, cex = 1.5, ft=2)
  
  # Precipitation contingency
  with(finalDF, boxplot(precM ~ lab, xlab = "Regions", 
                        ylab = "Precipitation contingency",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -8, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.42, line = -2, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.65, line = -1.2, cex = 1.5, ft=2)
  mtext("d", side = 3, adj = 0.85, line = -7.5, cex = 1.5, ft=2)
}

####################################################################################
Australia_compare_7 <- function(inDF) {
  
  #library
  require(agricolae)
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 7)
  globDF <- subset(inDF, BIOME == 7)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  #  # visually check
    #with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
    #                            xlab = "Precipitation",cex.lab=2, cex.main=2,
    #                            xlim=c(-180, 180), ylim=c(-90, 90),
    #                            nx=400, ny=120))
    #world(add=T, col=adjustcolor("grey", 0.5))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= 0 & lat >= -45)
  AU$lab <- "AU"
  # Africa
  AF <- subset(subDF.prec, lon <= 50 & lon >= -30 & lat <= 20 & lat >= -45)
  AF$lab <- "AF"
  # South America
  SA <- subset(subDF.prec, lon <= -31 & lon >= -100 & lat <= 0 & lat >= -50)
  SA$lab <- "SA"
  
  finalDF <- rbind(AU, AF, SA)
  finalDF$lab <- factor(finalDF$lab)
  
  # statistical test
  mod1 <- lm(tempP~lab, data=finalDF)
  summary(mod1)
  anova(mod1)
  confint(mod1)
  aov.out1 <- aov(tempP~lab, data=finalDF)
  out1 <- HSD.test(aov.out1, "lab")
  
  aov.out2 <- aov(precP~lab, data=finalDF)
  out2 <- HSD.test(aov.out2, "lab")
  
  # Plotting
  set.panel()
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -8.3, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.50, line = -8, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.8, line = -8, cex = 1.5, ft=2)

  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.2, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.52, line = -4, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.85, line = -1.2, cex = 1.5, ft=2)
}

####################################################################################
Australia_compare_8 <- function(inDF) {
  
  #library
  require(agricolae)
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 8)
  globDF <- subset(inDF, BIOME == 8)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  #  # visually check
  #with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
  #                            xlab = "Precipitation",cex.lab=2, cex.main=2,
  #                            xlim=c(-180, 180), ylim=c(-90, 90),
  #                            nx=400, ny=120))
  #world(add=T, col=adjustcolor("grey", 0.5))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= 0 & lat >= -45)
  AU$lab <- "AU"
  # US
  US <- subset(subDF.prec, lon <= -50 & lon >= -150 & lat <= 50 & lat >= 0)
  US$lab <- "US"
  # South America
  SA <- subset(subDF.prec, lon <= -50 & lon >= -100 & lat <= 0 & lat >= -50)
  SA$lab <- "SA"
  
  finalDF <- rbind(AU, US, SA)
  finalDF$lab <- factor(finalDF$lab)
  
  # statistical test
  mod1 <- lm(tempP~lab, data=finalDF)
  summary(mod1)
  anova(mod1)
  confint(mod1)
  aov.out1 <- aov(tempP~lab, data=finalDF)
  out1 <- HSD.test(aov.out1, "lab")
  
  aov.out2 <- aov(precP~lab, data=finalDF)
  out2 <- HSD.test(aov.out2, "lab")
  
  # Plotting
  set.panel()
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.22, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.50, line = -3.2, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.82, line = -2.5, cex = 1.5, ft=2)
  
  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.2, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.54, line = -1.4, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.85, line = -1.2, cex = 1.5, ft=2)
}

####################################################################################
Australia_compare_12 <- function(inDF) {
  
  #library
  require(agricolae)
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 12)
  globDF <- subset(inDF, BIOME == 12)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  #  # visually check
  #with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
  #                            xlab = "Precipitation",cex.lab=2, cex.main=2,
  #                            xlim=c(-180, 180), ylim=c(-90, 90),
  #                            nx=400, ny=120))
  #world(add=T, col=adjustcolor("grey", 0.5))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= 0 & lat >= -45)
  AU$lab <- "AU"
  # MD
  MD <- subset(subDF.prec, lon <= 50 & lon >= -50 & lat <= 50 & lat >= 0)
  MD$lab <- "MD"

  finalDF <- rbind(AU, MD)
  finalDF$lab <- factor(finalDF$lab)
  
  # statistical test
  mod1 <- lm(tempP~lab, data=finalDF)
  summary(mod1)
  anova(mod1)
  confint(mod1)
  aov.out1 <- aov(tempP~lab, data=finalDF)
  out1 <- HSD.test(aov.out1, "lab")
  
  aov.out2 <- aov(precP~lab, data=finalDF)
  out2 <- HSD.test(aov.out2, "lab")
  
  # Plotting
  set.panel()
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.3, line = -1.4, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.8, line = -1.6, cex = 1.5, ft=2)

  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.3, line = -1.6, cex = 1.5, ft=2)
  mtext("a", side = 3, adj = 0.8, line = -1.4, cex = 1.5, ft=2)
}

####################################################################################
Australia_compare_13 <- function(inDF) {
  
  #library
  require(agricolae)
  
  # read in file
  myDF <- subset(inDF, lon <= 155 & lon >= 110 & lat <= -12 & lat >= -45)
  myDF1 <- subset(myDF, BIOME == 13)
  globDF <- subset(inDF, BIOME == 13)
  
  # looking for similar grids based on temp min and max
  temp.min <- min(myDF1$temp)
  temp.max <- max(myDF1$temp)
  subDF.temp <- subset(globDF, temp >= temp.min & temp <= temp.max)
  dim(subDF.temp)
  
  # looking for similar grids based on additional prec min and max
  prec.min <- min(myDF1$prec_sum)
  prec.max <- max(myDF1$prec_sum)
  subDF.prec <- subset(subDF.temp, prec_sum >= prec.min & prec_sum <= prec.max)
  dim(subDF.prec)
  
  #  # visually check
  #with(subDF.prec, quilt.plot(lon, lat, prec_sum, 
  #                            xlab = "Precipitation",cex.lab=2, cex.main=2,
  #                            xlim=c(-180, 180), ylim=c(-90, 90),
  #                            nx=400, ny=120))
  #world(add=T, col=adjustcolor("grey", 0.5))
  
  # Subsetting different regions
  
  # Oz
  AU <- subset(subDF.prec, lon <= 155 & lon >= 110 & lat <= 0 & lat >= -45)
  AU$lab <- "AU"
  # ME
  ME <- subset(subDF.prec, lon <= 80 & lon >= 0 & lat <= 50 & lat >= 0)
  ME$lab <- "ME"
  # US
  US <- subset(subDF.prec, lon <= -50 & lon >= -150 & lat <= 50 & lat >= 0)
  US$lab <- "US"
  # AF
  AF <- subset(subDF.prec, lon <= 50 & lon >= 0 & lat <= 50 & lat >= -50)
  AF$lab <- "AF"
  
  finalDF <- rbind(AU, ME, US, AF)
  finalDF$lab <- factor(finalDF$lab)
  
  # statistical test
  mod1 <- lm(tempP~lab, data=finalDF)
  summary(mod1)
  anova(mod1)
  confint(mod1)
  aov.out1 <- aov(tempP~lab, data=finalDF)
  out1 <- HSD.test(aov.out1, "lab")
  
  aov.out2 <- aov(precP~lab, data=finalDF)
  out2 <- HSD.test(aov.out2, "lab")
  
  # Plotting
  set.panel()
  set.panel(2,1)
  
  # Temperature predictability
  with(finalDF, boxplot(tempP ~ lab, xlab = "Regions", 
                        ylab = "Temperature predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.4, line = -1.6, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.66, line = -1.6, cex = 1.5, ft=2)
  mtext("d", side = 3, adj = 0.85, line = -1.6, cex = 1.5, ft=2)
  
  # Precipitation predictability
  with(finalDF, boxplot(precP ~ lab, xlab = "Regions", 
                        ylab = "Precipitation predictability",
                        col=c("blue","red","yellow","cyan"),
                        cex.lab=1.2, cex.axis=1.2))
  mtext("a", side = 3, adj = 0.18, line = -1.6, cex = 1.5, ft=2)
  mtext("b", side = 3, adj = 0.4, line = -3, cex = 1.5, ft=2)
  mtext("a", side = 3, adj = 0.66, line = -3, cex = 1.5, ft=2)
  mtext("c", side = 3, adj = 0.85, line = -3, cex = 1.5, ft=2)
}

####################################################################################
AusExtract <- function(inDF) {
  
  ## Australia shapefile source:
  ## http://www.diva-gis.org/datadown
  
  # Library
  #require(PBSmapping)
  #require(oz)
  require(maptools)
  
  # read in data
  data <- readShapeSpatial("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/Australia/AUS_adm1")
  
  plot(data, lwd=0.01,bg="#242424",col="#0000CD",ylim=c(-46,-10),xlim=c(125,145))


}
  
####################################################################################
## Calculate coefficient of variation
CoefVar <- function(inDF) {
  # Calculate:
  # monthly coef of var
  # inter-annual coef of var
  # intra-annual coef of var
  
  # create a dtataframe to contain all the data
  outDF <- subset(inDF, year == 1901)
  outDF <- outDF[,1:3]
  
  site.list <- unique(inDF$CRU_Site)
  
  # calculate sd and mean all the above mentioned time period
  for (i in site.list)
  {
    subDF <- subset(inDF, CRU_Site == i)
    
    # monthly sd and mean
    outDF[outDF$CRU_Site == i, "mon_sd"] <- sd(as.matrix(subDF[,5:16]))
    outDF[outDF$CRU_Site == i, "mon_mean"] <- mean(as.matrix(subDF[,5:16]))
    
    # inter-annual sd and mean
    subDF$annual <- rowMeans(subDF[5:16])
    outDF[outDF$CRU_Site == i, "inter_mean"] <- mean(subDF$annual)
    outDF[outDF$CRU_Site == i, "inter_sd"] <- sd(subDF$annual)
    
    # intra-annual sd and mean
    intra <- colMeans(subDF[,5:16])
    outDF[outDF$CRU_Site == i, "intra_mean"] <- mean(intra)
    outDF[outDF$CRU_Site == i, "intra_sd"] <- sd(intra)
  }
  
  # calculate coef of var
  outDF$mon_coef <- outDF$mon_sd/outDF$mon_mean
  outDF$inter_coef <- outDF$inter_sd/outDF$inter_mean
  outDF$intra_coef <- outDF$intra_sd/outDF$intra_mean
  
  return(outDF)
  
}


####################################################################################
# Plotting summary profile using 2-dimensional error bars
summary2dimage <- function(summary) {
  
  # setting graphics
  require(psych)
  m <- matrix(c(1,2,3,3), nrow=2, ncol=2,byrow=T)
  layout(mat=m, heights=c(0.8,0.2))
  
  ## tempP vs. precP
  df1 <- data.frame(mean=summary$tempP_mean, sd=summary$tempP_sd)
  df2 <- data.frame(mean=summary$precP_mean, sd=summary$precP_sd)
  error.crosses(df1, df2, sd=T, color=color.list,
                xlab = "Temperature P", ylab = "Precipitation P",
                main=NA, cex.lab = 1.5)
  mtext("(a)", side = 3, adj = 0.05, line = -2, cex = 2)
  
  x.lab <- expression("Temperature [" * degree~C * "]")
  ## temp vs. prec
  df1 <- data.frame(mean=summary$temp_mean, sd=summary$temp_sd)
  df2 <- data.frame(mean=summary$prec_mean, sd=summary$prec_sd)
  error.crosses(df1, df2, sd=T, color=color.list, xlab=x.lab,
                ylab="Precipitation [mm]", main=NA, cex.lab = 1.5)
  mtext("(b)", side = 3, adj = 0.05, line = -2, cex = 2)
  
  # legend
  par(mar = rep(2, 4))
  plot(1, type="n", axes=F, xlab="", ylab="")
  legend(x="bottom",inset=0, legend = biomeN, fill=color.list, 
         cex=1,ncol=4)
  
  
  par(opar)
}

####################################################################################
pdfTOpng <- function(FileDir) {
  
  # library
  require(animation)
  Flist <- list.files(path = FileDir, pattern = ".pdf")
  
  Olist <- gsub("pdf", "jpeg", Flist)
  
  for (i in 1:length(Flist)) {
    im.convert(paste(FileDir, Flist[i], sep=""),
               output = paste(FileDir, Olist[i], sep=""))
  }
  
}
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

####################################################################################
##Function to calculate Colwell index for temperature
##112 years of monthly data, absolute values, 12 bin sizes
##Classification scheme: -3 stdev + mean to +3 stdev + mean
PCM_temp<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    #require(data.table)
    
    inName <- paste(sourceDir, "temp_DF.csv",sep="/")
    outName <- paste(destDir, "temp_PCM.csv", sep="/")
    
    input <- fread(inName, sep=",", header=T)
    input <- as.data.frame(input)
    
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
    
    
    years <- min(input$year)
    yeare <- max(input$year)
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
    
    col_sum <- 112
    whole_sum <- col_sum*12
    
    #uncertainty with respect to time H(X)
    HofX <- -((col_sum/whole_sum)*log10(col_sum/whole_sum))*12
    
    for (i in 1:nrow(temp))
    {
        
        X <- input[input$CRU_Site == i,]
        
        bin[,"jan"] = table(cut(X$jan, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"feb"] = table(cut(X$feb, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"mar"] = table(cut(X$mar, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"apr"] = table(cut(X$apr, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"may"] = table(cut(X$may, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"jun"] = table(cut(X$jun, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"jul"] = table(cut(X$jul, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"aug"] = table(cut(X$aug, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"sep"] = table(cut(X$sep, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"oct"] = table(cut(X$oct, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"nov"] = table(cut(X$nov, breaks, include.lowest=TRUE,right=TRUE))
        bin[,"dec"] = table(cut(X$dec, breaks, include.lowest=TRUE,right=TRUE))
        
        newbin <- as.numeric(bin[,2:13])
        newbin2 <- matrix(newbin,nrow=interval,ncol=12)
        newbin3 <- matrix(nrow=interval,ncol=1)
        for (n in 1:interval)
        {
            newbin3[n,] = sum(newbin2[n,1:12])
        }
        newbin <- cbind(newbin2,newbin3)
        
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


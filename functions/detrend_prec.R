####################################################################################
##Function to detrend the precipitation data
prec_detrend<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    #require(data.table)
    
    inName <- paste(sourceDir, "pre_DF.csv",sep="/")
    outName <- paste(destDir, "prec_detrended.csv", sep="/")
    
    input <- fread(inName, sep=",", header=T)

    # split the input df according to CRU site information
    inDF.spl <- split(input, f=input$CRU_Site)
    
    # get the number of grid
    l <- length(inDF.spl)
    
    # prepare output df
    outDF.spl <- inDF.spl
    
    # compute the detrending 
    for(i in 1:l) {
        tempDF <- as.data.frame(inDF.spl[[i]])
        for (j in 5:16) {
            outDF.spl[[i]][,j] <- detrend(unlist(tempDF[,j]))
        }
    }
    
    # merge the df
    output <- do.call("rbind", outDF.spl)
    
    # write output
    write.table(output,outName,sep=",",row.names=F)
}


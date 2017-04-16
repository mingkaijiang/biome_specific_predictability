####################################################################################
##Function to detrend the temperature data
temp_detrend<-function(sourceDir = DAILY.DATA.DIRECTORY, destDir = DAILY.OUTPUT.DIRECTORY)
{
    #require(data.table)
    
    inName <- paste(sourceDir, "temp_DF.csv",sep="/")
    outName <- paste(destDir, "temp_detrended.csv", sep="/")
    
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
    
    # write output
    write.table(output,outName,sep=",",row.names=F)
}


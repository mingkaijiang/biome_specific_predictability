####################################################################################
## Remove duplicated entries in the raw data
remove_duplicate <- function(inFile, outFile) {
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
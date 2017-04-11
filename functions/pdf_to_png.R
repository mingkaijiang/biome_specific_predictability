
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
#### Convert nc file into csv file for CRU temperature and precipitation data
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
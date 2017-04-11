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
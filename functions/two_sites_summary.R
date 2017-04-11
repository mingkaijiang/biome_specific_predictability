
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
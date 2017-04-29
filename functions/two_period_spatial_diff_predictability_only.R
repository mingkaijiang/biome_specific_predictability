
####################################################################################
two_period_spatial_diff_P <- function() {
    
    ### read in all the data (two periods temperature and precipitation)
    temp_p1 <- read.csv(paste0(dataDir, "/temp_PCM_1901_1990.csv"))
    temp_p2 <- read.csv(paste0(dataDir, "/temp_PCM_1991_2012.csv"))
    prec_p1 <- read.csv(paste0(dataDir, "/pre_PCM_1901_1990.csv"))
    prec_p2 <- read.csv(paste0(dataDir, "/pre_PCM_1991_2012.csv"))
    
    
    ### Setting same color scaling for temperature and precipitation data
    temp_p1$P_lab <- round(temp_p1$P, 1)
    temp_p1$C_lab <- round(temp_p1$C, 1)
    temp_p1$M_lab <- round(temp_p1$M, 1)

    temp_p2$P_lab <- round(temp_p2$P, 1)
    temp_p2$C_lab <- round(temp_p2$C, 1)
    temp_p2$M_lab <- round(temp_p2$M, 1)
    
    prec_p1$P_lab <- round(prec_p1$P, 1)
    prec_p1$C_lab <- round(prec_p1$C, 1)
    prec_p1$M_lab <- round(prec_p1$M, 1)
    
    prec_p2$P_lab <- round(prec_p2$P, 1)
    prec_p2$C_lab <- round(prec_p2$C, 1)
    prec_p2$M_lab <- round(prec_p2$M, 1)
    
    temp_p_list <- c(unique(temp_p1$P_lab),unique(temp_p2$P_lab))
    temp_c_list <- c(unique(temp_p1$C_lab),unique(temp_p2$C_lab))
    temp_m_list <- c(unique(temp_p1$M_lab),unique(temp_p2$M_lab))
    
    prec_p_list <- c(unique(prec_p1$P_lab),unique(prec_p2$P_lab))
    prec_c_list <- c(unique(prec_p1$C_lab),unique(prec_p2$C_lab))
    prec_m_list <- c(unique(prec_p1$M_lab),unique(prec_p2$M_lab))
    
    temp_p_brk <- seq(min(temp_p_list):max(temp_p_list), by = 0.1)
    temp_c_brk <- seq(0,1, by = 0.1)
    temp_m_brk <- seq(min(temp_m_list):max(temp_m_list), by = 0.1)
    
    prec_p_brk <- seq(min(prec_p_list):max(prec_p_list), by = 0.1)
    prec_c_brk <- seq(min(prec_c_list):max(prec_c_list), by = 0.1)
    prec_m_brk <- seq(min(prec_m_list):max(prec_m_list), by = 0.1)
    

    ### setting graphics
    par(xpd = T, mfrow=c(2,2), mar=c(4,4,4,7))
    
    ### figure: temperature P, C, M over two periods
    with(temp_p1, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Temperature Predictability", breaks=temp_p_brk, 
                             col=tim.colors((length(temp_p_brk)-1)),
                             main="1901-1990", add.legend=F))
    
    with(temp_p2, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab=NA, breaks=temp_p_brk, 
                             col=tim.colors((length(temp_p_brk)-1)),
                             main="1991-2012", add.legend=T))
    
    
    ### new row: precipitation P, C, M over two periods
    with(prec_p1, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Precipitation Predictability", breaks=prec_p_brk, 
                             col=tim.colors((length(prec_p_brk)-1)),
                             main=NA, add.legend=F))
    
    with(prec_p2, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab=NA, breaks=prec_p_brk, 
                             col=tim.colors((length(prec_p_brk)-1)),
                             main=NA, add.legend=T))
    
    
    
}
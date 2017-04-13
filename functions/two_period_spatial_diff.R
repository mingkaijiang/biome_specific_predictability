
####################################################################################
two_period_spatial_diff <- function() {
    
    ### read in all the data (two periods temperature and precipitation)
    temp_p1 <- read.csv(paste0(dataDir, "/temp_PCM_1901_1990.csv"))
    temp_p2 <- read.csv(paste0(dataDir, "/temp_PCM_1991_2012.csv"))
    prec_p1 <- read.csv(paste0(dataDir, "/pre_PCM_1901_1990.csv"))
    prec_p2 <- read.csv(paste0(dataDir, "/pre_PCM_1991_2012.csv"))
    
    ### compute differences
    temp_p3 <- temp_p1[,1:3]
    temp_p3$P_abs_diff <- temp_p2$P - temp_p1$P
    temp_p3$P_pct_diff <- (temp_p2$P - temp_p1$P)/temp_p1$P
    
    temp_p3$C_abs_diff <- temp_p2$C - temp_p1$C
    temp_p3$C_pct_diff <- (temp_p2$C - temp_p1$C)/temp_p1$C
    
    temp_p3$M_abs_diff <- temp_p2$M - temp_p1$M
    temp_p3$M_pct_diff <- (temp_p2$M - temp_p1$M)/temp_p1$M
    
    
    prec_p3 <- prec_p1[,1:3]
    prec_p3$P_abs_diff <- prec_p2$P - prec_p1$P
    prec_p3$P_pct_diff <- (prec_p2$P - prec_p1$P)/prec_p1$P
    
    prec_p3$C_abs_diff <- prec_p2$C - prec_p1$C
    prec_p3$C_pct_diff <- (prec_p2$C - prec_p1$C)/prec_p1$C
    
    prec_p3$M_abs_diff <- prec_p2$M - prec_p1$M
    prec_p3$M_pct_diff <- (prec_p2$M - prec_p1$M)/prec_p1$M
    
    #prec_p3[is.na(prec_p3)] <- -1.0
    
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
    par(xpd = T, mfrow=c(3,2), mar=c(4,4,4,7))
    
    ### figure: temperature P, C, M over two periods
    with(temp_p1, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Predictability", breaks=temp_p_brk, 
                             col=tim.colors((length(temp_p_brk)-1)),
                             main="Temperature 1901-1990", add.legend=T))
    
    with(temp_p2, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Predictability", breaks=temp_p_brk, 
                             col=tim.colors((length(temp_p_brk)-1)),
                             main="Temperature 1991-2012", add.legend=T))
    
    with(temp_p1, quilt.plot(lon, lat, C, nx=300, ny=260,
                             ylab="Constancy", breaks=temp_c_brk, 
                             col=tim.colors((length(temp_c_brk)-1)),
                             add.legend=T))
    
    with(temp_p2, quilt.plot(lon, lat, C, nx=300, ny=260,
                             ylab="Constancy", breaks=temp_c_brk, 
                             col=tim.colors((length(temp_c_brk)-1)),
                             add.legend=T))
    
    with(temp_p1, quilt.plot(lon, lat, M, nx=300, ny=260,
                             ylab="Contingency", breaks=temp_m_brk, 
                             col=tim.colors((length(temp_m_brk)-1)),
                             add.legend=T))
    
    with(temp_p2, quilt.plot(lon, lat, M, nx=300, ny=260,
                             ylab="Contingency", breaks=temp_m_brk, 
                             col=tim.colors((length(temp_m_brk)-1)),
                             add.legend=T))
    
    
    ### another figure: precipitation P, C, M over two periods
    with(prec_p1, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Predictability", breaks=prec_p_brk, 
                             col=tim.colors((length(prec_p_brk)-1)),
                             main="Precipitation 1901-1990", add.legend=T))
    
    with(prec_p2, quilt.plot(lon, lat, P, nx=300, ny=260,
                             ylab="Predictability", breaks=prec_p_brk, 
                             col=tim.colors((length(prec_p_brk)-1)),
                             main="Precipitation 1991-2012", add.legend=T))
    
    with(prec_p1, quilt.plot(lon, lat, C, nx=300, ny=260,
                             ylab="Constancy", breaks=prec_c_brk, 
                             col=tim.colors((length(prec_c_brk)-1)),
                             add.legend=T))
    
    with(prec_p2, quilt.plot(lon, lat, C, nx=300, ny=260,
                             ylab="Constancy", breaks=prec_c_brk, 
                             col=tim.colors((length(prec_c_brk)-1)),
                             add.legend=T))
    
    with(prec_p1, quilt.plot(lon, lat, M, nx=300, ny=260,
                             ylab="Contingency", breaks=prec_m_brk, 
                             col=tim.colors((length(prec_m_brk)-1)),
                             add.legend=T))
    
    with(prec_p2, quilt.plot(lon, lat, M, nx=300, ny=260,
                             ylab="Contingency", breaks=prec_m_brk, 
                             col=tim.colors((length(prec_m_brk)-1)),
                             add.legend=T))
    
    ### new figure: absolute differences
    ### temperature P abs diff
    with(temp_p3, quilt.plot(lon, lat, P_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="predictability",
                             main="Temperature", add.legend=T))
    
    ### precipitation P abs diff
    with(prec_p3, quilt.plot(lon, lat, P_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="predictability",
                             main="Precipitation", add.legend=T))
    
    ### temperature C abs diff
    with(temp_p3, quilt.plot(lon, lat, C_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="constancy",
                             add.legend=T))
    
    ### precipitation C abs diff
    with(prec_p3, quilt.plot(lon, lat, C_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="constancy",
                             add.legend=T))
    
    ### temperature M abs diff
    with(temp_p3, quilt.plot(lon, lat, M_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="contingency",
                             add.legend=T))
    
    ### precipitation M abs diff
    with(prec_p3, quilt.plot(lon, lat, M_abs_diff, nx=300, ny=260, nlevel=12,
                             ylab="contingency",
                             add.legend=T))
    
    par(opar)
    
    par(xpd=T,mfrow=c(2,1), mar=c(4,4,4,7))
    
    ### new figure: percent differences
    ### temperature P pct diff
    with(temp_p3, quilt.plot(lon, lat, P_pct_diff, nx=300, ny=260, nlevel=12,
                             ylab="predictability", na.omit=T,
                             main="Temperature", add.legend=T))
    
    ### precipitation P pct diff
    with(prec_p3, quilt.plot(lon, lat, P_pct_diff, nx=300, ny=260, nlevel=12,
                             ylab="predictability", na.omit=T,
                             main="Precipitation", add.legend=T))

    
    
    par(opar)
    
}
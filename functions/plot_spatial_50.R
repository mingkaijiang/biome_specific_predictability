
####################################################################################
## plot 50th percentile spatial data
spatial50 <- function(absDF, pDF) {
    
    # Set output graph structure
    par(oma=c(2,2,2,2),
        mar=c(5.1,5.1,5.1,5.1),
        mgp = c(3, 1, 0))
    set.panel(2,2)
    
    ## abs climate 50th percentile
    with(absDF, quilt.plot(lon, lat, temp, main="Mean annual temperature",
                           nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    with(absDF, quilt.plot(lon, lat, prec_sum, main="Sum annual precipitation",
                           nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    ## predictability climate 50th percentile
    with(pDF, quilt.plot(lon, lat, tempP, main="Temperature predictability",
                         nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
    
    with(pDF, quilt.plot(lon, lat, precP, main="Precipitation predictability",
                         nx=300, ny=260, nlevel=12))
    world(add=T, col=adjustcolor("grey", 0.5))
} 
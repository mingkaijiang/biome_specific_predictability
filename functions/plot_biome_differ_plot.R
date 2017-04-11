
####################################################################################
BiomeDifferPlot <- function(inDF) {
    # Check if P helps to differ biomes with similar Temp and Prec profiles
    
    for (i in 1:14) {
        for (j in 1:14) {
            max.prec1 <- max(inDF[inDF$BIOME == i,"prec_sum"])
            max.prec2 <- max(inDF[inDF$BIOME == j,"prec_sum"])
            
            min.prec1 <- min(inDF[inDF$BIOME == i,"prec_sum"])
            min.prec2 <- min(inDF[inDF$BIOME == j,"prec_sum"])
            
            max.temp1 <- max(inDF[inDF$BIOME == i,"temp"])
            max.temp2 <- max(inDF[inDF$BIOME == j,"temp"])
            
            min.temp1 <- min(inDF[inDF$BIOME == i,"temp"])
            min.temp2 <- min(inDF[inDF$BIOME == j,"temp"])
            
            max.prec3 <- min(max.prec1, max.prec2)
            min.prec3 <- max(min.prec1, min.prec2)
            
            max.temp3 <- min(max.temp1, max.temp2)
            min.temp3 <- max(min.temp1, min.temp2)
            
            newDF <- subset(inDF, BIOME == i | BIOME == j)
            newDF <- subset(newDF, prec_sum <= max.prec3 & prec_sum >= min.prec3)
            newDF <- subset(newDF, temp <= max.temp3 & temp >= min.temp3)
            
            if (dim(newDF)[1] > 0) {plotBoxplot(newDF)}
            
        }
    }
}

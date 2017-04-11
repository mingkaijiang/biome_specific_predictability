
plotBoxplot <- function(newDF) {
    # graphics
    m <- matrix(c(1,4,2,5,3,6), nrow=3, ncol=2,byrow=T)
    layout(mat=m, heights=c(0.33, 0.33, 0.33))
    
    # plotting
    with(newDF, boxplot(tempP~BIOME, ylab = "Temperature predictability",
                        main = "Temperature"))
    with(newDF, boxplot(tempC~BIOME, ylab = "Temperature constancy"))
    with(newDF, boxplot(tempM~BIOME, ylab = "Temperature contingency"))
    
    with(newDF, boxplot(precP~BIOME, ylab = "Precipitation predictability",
                        main = "Precipitation"))
    with(newDF, boxplot(precC~BIOME, ylab = "Precipitation constancy"))
    with(newDF, boxplot(precM~BIOME, ylab = "Precipitation contingency"))
}

cohesive_animated_plots <- function(plotDF) {
    
    # set wd
    densityPath <- paste0(animatedDir, "/density")
    if(!dir.exists(densityPath)) {
        dir.create(densityPath, showWarnings = FALSE)
    }
    setwd(densityPath)
    
    # Plot animated density plot
    AnimatedDensityPlot(plotDF)
    # convert into animation
    system("convert -delay 100 *.png animated.gif")
    
    #  setwd
    radarPath <- paste0(animatedDir, "/radar")
    if(!dir.exists(radarPath)) {
        dir.create(radarPath, showWarnings = FALSE)
    }
    setwd(radarPath)
    
    # Plot animated radar plot
    Animated_radar(summary)
    
    # convert into animation
    system("convert -delay 100 *.png animated.gif")
    
    
    #  setwd
    setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/PCA")
    ## convert into animation
    system("convert -delay 100 *.png animated.gif")
    
    #  setwd
    setwd("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/CRU/animated/IE")
    ## convert into animation
    system("convert -delay 100 *.png animated.gif")
}



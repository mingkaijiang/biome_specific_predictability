
####################################################################################
AusExtract <- function(inDF) {
    
    ## Australia shapefile source:
    ## http://www.diva-gis.org/datadown
    
    # Library
    #require(PBSmapping)
    #require(oz)
    require(maptools)
    
    # read in data
    data <- readShapeSpatial("/Users/mingkaijiang/Documents/Predictability_Project/P4_Literature_review/data/Australia/AUS_adm1")
    
    plot(data, lwd=0.01,bg="#242424",col="#0000CD",ylim=c(-46,-10),xlim=c(125,145))
    
    
}

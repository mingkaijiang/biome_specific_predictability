####################################################################################
ozflux_extraction <- function(cru) {
    
    # read table
    ozsite <- read.table("/Users/mingkaijiang/Documents/PostDoc/OzFlux/Site_coordinates.csv",
                         header=T,sep=",")
    
    # Project ozflux grids to CRU grids
    ozcru <- oz_to_cru(ozsite)
    cru.name <- names(cru)
    
    # Extract CRU climate information
    for (i in unique(ozcru$LON_CRU)) {
        for (j in unique(ozcru$LAT_CRU)) {
            
            # checking data point presence in cru dataframe
            grid.check <- length(cru[cru$lon == i & cru$lat == j, "temp"])
            
            # assign values onto all columns
            for (n in 1: length(cru.name)) {
                ozcru[ozcru$LON_CRU == i & ozcru$LAT_CRU == j, cru.name[n]] <- ifelse(grid.check > 0, 
                                                                                      cru[cru$lon == i & cru$lat == j, cru.name[n]],
                                                                                      NA)
            } # n
        } # j
    }  # i
    
    return(ozcru)
    
}
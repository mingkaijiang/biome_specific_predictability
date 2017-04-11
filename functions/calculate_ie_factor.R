####################################################################################
## Calculate ie factor for temperature and precipitation
iefactor <- function(inDF) {
    
    #IE for temperature
    inDF$tempMC <- inDF$tempM/inDF$tempC
    inDF$tempIE <- ifelse(inDF$tempMC >=1, 2, 1)
    
    #IE for precipitation
    inDF$precMC <- inDF$precM/inDF$precC
    inDF$precIE <- ifelse(inDF$precMC >=1, 2, 1)
    
    return(inDF)
    
}

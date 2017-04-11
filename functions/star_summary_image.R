
####################################################################################
## Plot star plots
star_summary_image <- function(inDF) {
    
    # library
    #require(fmsb)
    
    # prepare dataframe
    col.list <- color.list
    newDF <- data.frame(inDF$temp_mean, inDF$prec_mean,
                        inDF$tempP_mean,inDF$precP_mean)
    names(newDF) <- c("temp", "prec", "temp P", "prec P")
    rownames(newDF) <- biome
    
    # plotting
    stars(newDF, len = 0.8, key.loc = c(6.8, 2.3),
          draw.segments = T, 
          nrow=5, ncol=3,
          col.segments=c("red","blue","yellow","orange"))
    
}
####################################################################################
# sub function in BiomeDifferStats_comparison
# two time period comparison
plotMatrixCol_comparison <- function(inDF1, inDF2, title.name) {
    
    # library
    #library(gplots)
    
    # need data as matrix
    #mm <- matrix(rexp(196), 14)
    mm1 <- inDF1
    mm2 <- inDF2
    
    heatmap.2(x = mm1, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
              col=c("white","lightgrey","red","blue","yellow","green"),
              colsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
              rowsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
              sepcolor="black",sepwidth=c(0.02,0.02),
              trace = "none", key = F,key.title = title.name,
              margins = c(7, 11), main = "1901-1990")
    
    legend(0,1, legend=c("no overlap", "temp P", "prec P", "both", "neither"),
           fill=c("lightgrey","red","blue","yellow","green"),cex=.8)

    heatmap.2(x = mm2, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
              col=c("white","lightgrey","red","blue","yellow","green"),
              colsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
              rowsep=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14),
              sepcolor="black",sepwidth=c(0.02,0.02),
              trace = "none", key = F,key.title = title.name,
              margins = c(7, 11), main = "1991-2012")
    
    legend(0,1, legend=c("no overlap", "temp P", "prec P", "both", "neither"),
           fill=c("lightgrey","red","blue","yellow","green"),cex=.8)
    
}

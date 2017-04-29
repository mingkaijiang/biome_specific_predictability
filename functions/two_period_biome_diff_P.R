
####################################################################################
two_period_biome_diff_P <- function() {
    
    ### read in all the data (two periods temperature and precipitation)
    temp_p1 <- read.csv(paste0(dataDir, "/temp_PCM_1901_1990.csv"))
    temp_p2 <- read.csv(paste0(dataDir, "/temp_PCM_1991_2012.csv"))
    prec_p1 <- read.csv(paste0(dataDir, "/pre_PCM_1901_1990.csv"))
    prec_p2 <- read.csv(paste0(dataDir, "/pre_PCM_1991_2012.csv"))
    
    ### read in biome coordinates
    corFile=read.csv(paste0(corDir, "/CRU_Biome.csv"))
    
    ### assign biomes
    temp_p1$biome <- corFile$BIOME
    temp_p2$biome <- corFile$BIOME
    prec_p1$biome <- corFile$BIOME
    prec_p2$biome <- corFile$BIOME
    
    ### assign period indication
    temp_p1$period <- "1st"
    temp_p2$period <- "2nd"
    prec_p1$period <- "1st"
    prec_p2$period <- "2nd"
    
    # prepare dataframe
    temp_p1<- subset(temp_p1, biome > 0)
    temp_p2<- subset(temp_p2, biome > 0)
    prec_p1<- subset(prec_p1, biome > 0)
    prec_p2<- subset(prec_p2, biome > 0)
    
    temp_p1 <- subset(temp_p1, biome < 98)
    temp_p2 <- subset(temp_p2, biome < 98)
    prec_p1 <- subset(prec_p1, biome < 98)
    prec_p2 <- subset(prec_p2, biome < 98)
    
    ### create long dfs
    temp_long <- as.data.frame(rbind(cbind(temp_p1$biome, temp_p1$period, temp_p1$P, temp_p1$C, temp_p1$M),
                               cbind(temp_p2$biome, temp_p2$period, temp_p2$P, temp_p2$C, temp_p2$M)))
    colnames(temp_long) <- c("biome", "period", "predictability", "constancy", "contingency")
    temp_long$predictability <- as.numeric(as.character(temp_long$predictability))
    temp_long$constancy <- as.numeric(as.character(temp_long$constancy))
    temp_long$contingency <- as.numeric(as.character(temp_long$contingency))

    
    prec_long <- as.data.frame(rbind(cbind(prec_p1$biome, prec_p1$period, prec_p1$P, prec_p1$C, prec_p1$M),
                                     cbind(prec_p2$biome, prec_p2$period, prec_p2$P, prec_p2$C, prec_p2$M)))
    colnames(prec_long) <- c("biome", "period", "predictability", "constancy", "contingency")
    prec_long$predictability <- as.numeric(as.character(prec_long$predictability))
    prec_long$constancy <- as.numeric(as.character(prec_long$constancy))
    prec_long$contingency <- as.numeric(as.character(prec_long$contingency))
    
    
    par(mfrow=c(2,1),pxd=T)

    ## Plotting
    boxplot(predictability ~ period*biome, data=temp_long, notch=TRUE, 
            col=(c("gold","darkgreen")), ylim=c(0,1),
            names=c(1,NA,2,NA,3,NA,4,NA,5,NA,6,NA,7,NA,8,NA,9,NA,10,NA,11,NA,
                    12,NA,13,NA,14,NA),
            #main="Biome-specific temperature P comparison of the two periods", 
            xlab="Biome", ylab="Temperature predictability")
    legend('bottomright', c("1901-1990", "1991-2012"), fill=c("gold", "darkgreen"))
    
    boxplot(predictability ~ period*biome, data=prec_long, notch=TRUE, 
            col=(c("gold","darkgreen")),  ylim=c(0,1),
            names=c(1,NA,2,NA,3,NA,4,NA,5,NA,6,NA,7,NA,8,NA,9,NA,10,NA,11,NA,
                    12,NA,13,NA,14,NA),
            #main="Biome-specific precipitation P comparison of the two periods", 
            xlab="Biome", ylab="Precipitation predictability")
    legend('bottomright', c("1901-1990", "1991-2012"), fill=c("gold", "darkgreen"))
    

}
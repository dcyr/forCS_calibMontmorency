###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
wwd <- "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/"
wwd <- paste(wwd, Sys.Date(), sep ="/")
dir.create(wwd)
setwd(wwd)


require(ggplot2)
require(dplyr)
require(tidyr)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha

simName <- "ForMont_test"



###################################################################
### summary variables - cell level
###################################################################
## fetching output
log_Summary <- get(load(paste0("../outputCompiled/log_Summary_",
                               simName, ".RData")))
nSims <- length(unique(log_Summary$replicate)) * length(unique(log_Summary$landtype))

#Pools
dfPools <- log_Summary %>%
    filter(variable %in% c("ABio",  "BBio", "TotalDOM")) 


dfPoolsPercentiles <- dfPools %>%
    group_by(areaName, treatment,
             # landtype
             initComm, growthShape,
             Time,  variable) %>%
    summarise(p25 = quantile(C_gPerSqM, 0.25),
              p50 = quantile(C_gPerSqM, 0.5),
              p75 = quantile(C_gPerSqM, 0.75),
              p05 = quantile(C_gPerSqM, 0.05),
              p95 = quantile(C_gPerSqM, 0.95),
              meanVal = mean(C_gPerSqM),
              maxVal = max(C_gPerSqM),
              minVal = min(C_gPerSqM))

# ## renaming initComm for nicer plotting
# initCommNewNames <- c(bor = "MRC Portneuf - Coniferous",
#                       temp = "MRC Papineau - Hardwoods")
# treatNewNames <- c(CPRS = "CPRS (100 yr)",
#                    CJ = "CJ (35 yr)",
#                    Firewood = "Firewood (35 yr)")
# df$initComm <- factor(initCommNewNames[as.character(df$initComm)],
#                       levels = initCommNewNames)
# dfPercentiles$initComm <- factor(initCommNewNames[as.character(dfPercentiles$initComm)],
#                                  levels = initCommNewNames)
# df$treatment <- factor(treatNewNames[as.character(df$treatment)],
#                        levels = treatNewNames)
# dfPercentiles$treatment <- factor(treatNewNames[as.character(dfPercentiles$treatment)],
#                                   levels = treatNewNames)



for(initC in unique(dfPools$initComm)) {
    df <- dfPools %>%
        filter(initComm == initC,
               growthShape %in% c(0.1, 0.5, 0.9))
    
    
    dfPercent <- dfPoolsPercentiles %>%
        filter(initComm == initC,
               growthShape %in% c(0.1, 0.5, 0.9))
  

    p <- ggplot(df, aes(x = Time, y = C_gPerSqM*unitConvFact,
                        colour = variable, fill = variable)) +
        #linetype = tenure, +
        theme_dark() +
        facet_grid(treatment ~ growthShape) +
        geom_line(data = dfPercent,
                  aes(x = Time, y = p50*unitConvFact),
                  #colour = "black",
                  size  = 0.5, alpha = 1) +
        # geom_line(data = dfPercentiles,
        #           aes(x = Time, y = meanVal*unitConvFact),
        #           size  = 0.5, alpha = 1) +
        geom_ribbon(data = dfPercent,
                    aes(x = Time, y = NULL, colour = NULL,
                        ymin = p05*unitConvFact, ymax = p95*unitConvFact),
                    alpha = 0.25) +
        geom_ribbon(data = dfPercent,
                    aes(x = Time, y = NULL, colour = NULL,
                        ymin = p25*unitConvFact, ymax = p75*unitConvFact),
                    alpha = 0.5) +
        # geom_line(data = filter(df, replicate %in% sample(unique(df$replicate), 1)),
        #           aes(x = Time, y = value*unitConvFact, group = group),
        #           size  = 0.2, alpha = 1, colour = "black") +
        scale_colour_manual(values = c("darkgreen","chocolate2", "coral4" )) +
        scale_fill_manual(values = c("darkgreen","chocolate2", "coral4" )) +
        theme(plot.caption = element_text(size = rel(.75), hjust = 0),
              plot.subtitle = element_text(size = rel(.75))) +
        labs(title = paste0("Summary of aggregated pools\nSpecies: ", initC),
             subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                               "\nMedians are represented with black lines",
                               "\n90% of values are comprised within lightly shaded areas",
                               "\n50% of values are comprised within darkly shaded areas."),
                               #"\nA random simulation is represented in black"),
             x = "",
             y = expression(paste("tonnes C"," ha"^"-1","\n")),
             caption = paste0("ABio : Aboveground biomass stocks",
                              "\nBBio : Belowground (root) biomass stocks",
                              "\nTotalDOM : Total dead organic matter and soil stocks (DOM + SOM)"))
    
    fName <- paste0("pools_Summary_", simName, "_", initC, ".png")
    
    png(filename= fName,
        width = 10, height = 6, units = "in", res = 600, pointsize=10)
    
        print(p)
    
    dev.off()
}

###################################################################
### Fluxes cell - level

log_Summary <- get(load(paste0("../outputCompiled/log_Summary_",
                               simName, ".RData")))
### pools
dfFluxes <- log_Summary %>%
    filter(variable %in% c("Turnover",  "NetGrowth", "NPP",
                           "Rh", "NEP"))


dfFluxesPercentiles <- dfFluxes %>%
    group_by(areaName, treatment,
             # landtype
             initComm, growthShape,
             Time,  variable) %>%
    summarise(p25 = quantile(C_gPerSqM, 0.25),
              p50 = quantile(C_gPerSqM, 0.5),
              p75 = quantile(C_gPerSqM, 0.75),
              p05 = quantile(C_gPerSqM, 0.05),
              p95 = quantile(C_gPerSqM, 0.95),
              meanVal = mean(C_gPerSqM),
              maxVal = max(C_gPerSqM),
              minVal = min(C_gPerSqM))


for(initC in unique(dfFluxes$initComm)) {
    df <- dfFluxes %>%
        filter(initComm == initC,
               growthShape %in% c(0.1, 0.5, 0.9))
    
    
    dfPercent <- dfFluxesPercentiles %>%
        filter(initComm == initC,
               growthShape %in% c(0.1, 0.5, 0.9))
    
    
    for (treat in c("CPRS", "CP")) {
        
        if(treat == "CPRS") {
            years <- c(425:550)
            zero <- 449
        } else {
            years <- c(440:565)
            zero <- 464
        }
        
        x <- df %>%
            filter(treatment == treat,
                   Time %in% years)
        
        xPercent <- dfPercent %>%
            filter(treatment == treat,
                   Time %in% years) 
        
        p <- ggplot(x, aes(x = Time - zero, y = C_gPerSqM*unitConvFact)) +
            theme_dark() +
            facet_grid(variable ~ growthShape,
                       scales = "free_y") +
            geom_line(data = xPercent,
                      aes(x = Time - zero, y = p50*unitConvFact),
                      size  = 0.5, alpha = 1, colour = "lightblue") +
            geom_ribbon(data = xPercent,
                        aes(x = Time - zero, y = NULL, colour = NULL, 
                            ymin = p05*unitConvFact, ymax = p95*unitConvFact),
                        alpha = 0.25, fill = "lightblue") +
            geom_ribbon(data = xPercent,
                        aes(x = Time - zero, y = NULL, colour = NULL,
                            ymin = p25*unitConvFact, ymax = p75*unitConvFact),
                        alpha = 0.5, fill = "lightblue") +
            geom_hline(yintercept = 0, colour = "grey25", linetype = 2, size = 0.3) +
            geom_vline(xintercept = 0, colour = "grey25", linetype = 2, size = 0.3) +
            theme(plot.caption = element_text(size = rel(.5), hjust = 0),
                  plot.subtitle = element_text(size = rel(.75))) +
            labs(title = paste0("Summary of ecosystem-level fluxes\nSpecies: ", initC, "\nTreatment: ",treat),
                 subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                                   "\nMedians are represented with blue lines",
                                   "\n90% of values are comprised within lightly shaded areas",
                                   "\n50% of values are comprised within darkly shaded areas."),
                 #"\nA random simulation is represented in black"),
                 x = "Years\n(relative to harvest)\n",
                 y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
                 caption = paste0("Turnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
                                  "\nNetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative as the stand ages and mortality outpaces growth.",
                                  "\nNPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
                                  "\nRh : Heterotrophic respiration. This is the sum of the 'To Air' fluxes through decomposition, not disturbance.",
                                  "\nNEP : Net Ecosystem Productivity. NPP minus Rh."
                 ))
        
        fName <- paste0("fluxes_Summary_", simName, "_", initC,  "_", treat, ".png")
        
        png(filename= fName,
            width = 8, height = 10, units = "in", res = 600, pointsize=10)
        
        print(p)
        
        dev.off()
    }
        
}


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
### Cumul emissions/removal - cell-level / ecosystem only
###################################################################
## fetching output
log_Summary <- get(load(paste0("../outputCompiled/log_Summary_",
                               simName, ".RData")))
nSims <- length(unique(log_Summary$replicate)) * length(unique(log_Summary$landtype))



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
            years <- c(425:650)
            zero <- 449
        } else {
            years <- c(440:665)
            zero <- 464
        }
        
        x <- dfPercent %>%
          filter(treatment == treat,
                 Time %in% years) %>%
          group_by(areaName, treatment, initComm, growthShape, variable) %>%
          arrange(Time) %>%
          mutate(cumulEmissions = cumsum(meanVal))
        
        
        
        
        
        
        
        
        p <- ggplot(x, aes(x = Time - zero, y = C_gPerSqM*unitConvFact)) +
            theme_dark() +
            facet_grid(variable ~ growthShape,
                       scales = "free_y") +
            geom_line(#data = xPercent,
                      aes(x = Time - zero, y = p50*unitConvFact),
                      size  = 0.5, alpha = 1, colour = "lightblue") +
            geom_ribbon(#data = xPercent,
                        aes(x = Time - zero, y = NULL, colour = NULL, 
                            ymin = p05*unitConvFact, ymax = p95*unitConvFact),
                        alpha = 0.25, fill = "lightblue") +
            geom_ribbon(#data = xPercent,
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


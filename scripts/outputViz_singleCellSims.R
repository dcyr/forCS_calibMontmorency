###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
wwd <- ifelse(Sys.info()["sysname"] == "Linux",
              paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep = "/"),
              "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/")
wwd <- paste(wwd, Sys.Date(), sep ="/")
dir.create(wwd)
setwd(wwd)


require(ggplot2)
require(dplyr)
require(tidyr)

unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha

simName <- c("ForMont_gs-ms")


gsSubset <- c(0.25, 0.5, 1)
gsSubsetLarge <- c(0.25, 0.5, 0.75, 0.9, 0.95, 1)

msSubset <- c(7, 15, 23)
msSubsetLarge <- c(7, 11, 15, 19, 23)

for (s in simName) { 
  ###################################################################
  ### summary variables - cell level
  ###################################################################
  ## fetching output
 
  log_Summary <- get(load(paste0("../outputCompiled/log_Summary_",
                                 s, ".RData")))
  nSims <- length(unique(log_Summary$replicate)) * length(unique(log_Summary$landtype))
  
  
  # ###################################################################
  # ###################################################################
  # ### very long, uncomment if update needed, else, read the csv
  # ###################################################################
  # #Pools
  # dfPools <- log_Summary %>%
  #     filter(variable %in% c("ABio",  "BBio", "TotalDOM")) 
  # 
  # 
  # dfPoolsPercentiles <- dfPools %>%
  #   group_by(areaName, treatment,
  #            # landtype
  #            initComm,
  #            growthShape, mortalityShape,
  #            Time,  variable) %>%
  #   summarise(p25 = quantile(C_gPerSqM, 0.25),
  #             p50 = quantile(C_gPerSqM, 0.5),
  #             p75 = quantile(C_gPerSqM, 0.75),
  #             p05 = quantile(C_gPerSqM, 0.05),
  #             p95 = quantile(C_gPerSqM, 0.95),
  #             meanVal = mean(C_gPerSqM),
  #             maxVal = max(C_gPerSqM),
  #             minVal = min(C_gPerSqM)) %>%
  #   ungroup()
  # 
  # 
  # 
  # ### write to file for calibration purposes
  # df <- dfPoolsPercentiles %>%
  #   #ungroup() %>%
  #   filter(variable == "ABio",
  #          initComm %in% c("ABIE.BAL", "PICE.GLA"),
  #          (treatment == "CPRS" & Time %in% c(450:575)) |
  #            (treatment == "CP" & Time %in% c(465:590))) %>%
  #   mutate(Time = ifelse(treatment == "CPRS", Time - 450,
  #                        Time - 465),
  #          ABio_C_TonnesPerHa = meanVal * unitConvFact) %>%
  #   arrange(initComm,
  #           growthShape, mortalityShape,
  #           Time) %>%
  #   select(areaName, treatment, initComm,
  #          growthShape, mortalityShape,
  #          Time, ABio_C_TonnesPerHa)
  #  
  # ###################################################################
  # ################################################################### 
  # write.csv(df, file = paste0("AGbio_output_", s, ".csv"), row.names = F)
  # write.csv(dfPoolsPercentiles, file = paste0("AGbio_summaryPercentiles_", s, ".csv"), row.names = F)
  # ###################################################################
  # ###################################################################
  
  dfPoolsPercentiles <- read.csv(file = paste0("../outputCompiled/AGbio_summaryPercentiles_", s, ".csv"))
  
  for(initC in unique(dfPoolsPercentiles$initComm)) {
    
    msLvls <- paste("mortalityShape =",
                    unique(dfPoolsPercentiles$mortalityShape))
    gsLvls <- paste("growthShape =",
                    unique(dfPoolsPercentiles$growthShape))
    
    df <- dfPoolsPercentiles %>%
      filter(initComm == initC,
             growthShape %in% gsSubset,
             mortalityShape %in% msSubset) %>%
      mutate(growthShape = factor(paste("growthShape =", growthShape),
                                  levels = gsLvls),
             mortalityShape = factor(paste("mortalityShape =", mortalityShape),
                                     levels = msLvls))
    
    p <- ggplot(df, aes(x = Time, y = C_gPerSqM*unitConvFact,
                        colour = variable,
                        fill = variable,
                        linetype = mortalityShape),
                group = paste(treatment, initComm, growthShape, mortalityShape, variable)) +
      #theme_dark() +
      facet_grid(treatment ~ growthShape) +
      # geom_ribbon(aes(x = Time, y = NULL, colour = NULL,
      #                 ymin = p05*unitConvFact, ymax = p95*unitConvFact),
      #             alpha = 0.25) +
      # geom_ribbon(aes(x = Time, y = NULL, colour = NULL,
      #                 ymin = p25*unitConvFact, ymax = p75*unitConvFact),
      #             alpha = 0.5) +
      geom_line(aes(x = Time, y = meanVal*unitConvFact),
                size  = 0.5, alpha = 1) +
      scale_colour_manual(name = "Carbon pool",
                          values = c("darkgreen","chocolate2", "coral4" )) +
      scale_fill_manual(name = "Carbon pool",
                        values = c("darkgreen","chocolate2", "coral4" )) +
      scale_linetype_manual(name = "",
                        values = rev(c(1,2,3))) +
      theme(plot.caption = element_text(size = rel(.75), hjust = 0),
            plot.subtitle = element_text(size = rel(.75))) +
      labs(title = paste0("Summary of aggregated pools\nSpecies: ", initC),
           subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                             "\nMean values are represented with full lines"#,
                             # "\n90% of values are comprised within shaded areas",
                             # "\n50% of values are comprised within darkly shaded areas."
                             ),
           x = "",
           y = expression(paste("tonnes C"," ha"^"-1","\n")),
           caption = paste0("ABio : Aboveground biomass stocks",
                            "\nBBio : Belowground (root) biomass stocks",
                            "\nTotalDOM : Total dead organic matter and soil stocks (DOM + SOM)"))
    
    fName <- paste0("pools_Summary_", s, "_", initC, ".png")
    
    png(filename= fName,
        width = 10, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)
    
    dev.off()
  }
  
  
  
  for(initC in unique(dfPoolsPercentiles$initComm)) {
    
    df <- dfPoolsPercentiles %>%
      filter(initComm == initC,
             variable == "ABio",
             Time <= 149,
             treatment == "CPRS",
             growthShape %in% gsSubsetLarge,
             mortalityShape %in% msSubsetLarge) %>%
      mutate(growthShape = factor(paste("growthShape =", growthShape),
                                  levels = gsLvls),
             mortalityShape = factor(paste("mortalityShape =", mortalityShape),
                                     levels = msLvls)) %>%
      group_by(growthShape, mortalityShape, Time) %>%
      summarise(p50 = mean(p50),
                meanVal = mean(meanVal))
    
    p <- ggplot(df, aes(x = Time, y = C_gPerSqM*unitConvFact,
                        linetype = mortalityShape, 
                        colour = growthShape)) +
      #theme_dark() +
      geom_line(aes(x = Time, y = meanVal*unitConvFact),
                size  = 0.5, alpha = 1) +
      # geom_vline(xintercept = 149.5, colour = "grey25", linetype = 2, size = 1) +
      theme(plot.caption = element_text(size = rel(.75), hjust = 0),
            plot.subtitle = element_text(size = rel(.75))) +
      # geom_text(label = "CPRS",
      #           x = 149.5, y = max(df$meanVal)*unitConvFact,
      #           angle = 270, vjust = -0.5, hjust = 0,
      #           colour = "grey25") +
      labs(title = paste0("Evolution of aboveground biomass stocks\nSpecies: ", initC),
           subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                             "\nLines represent average values values"),
           x = "",
           y = expression(paste("tonnes C"," ha"^"-1","\n")))
    
    fName <- paste0("AGB_Summary_", s, "_", initC, ".png")
    
    png(filename= fName,
        width = 10, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)
    
    dev.off()
  }
  
  
  
  
  # 
  # 
  # for(initC in unique(dfPools$initComm)) {
  #   
  #   df <- dfPoolsPercentiles %>%
  #     filter(initComm == initC,
  #            variable == "ABio",
  #            Time <= 150,
  #            growthShape %in% c(0.1, 0.3, 0.5, 0.7, 0.9)) %>%
  #            growthShape %in% seq(from = 0.8, to = 1, by = 0.02)) %>% 
  #            
  #     mutate(growthShape = factor(paste("growthShape =", growthShape)))
  #   
  #   p <- ggplot(df, aes(x = Time, y = C_gPerSqM*unitConvFact,
  #                       colour = treatment, fill = treatment)) +
  #     theme_dark() +
  #     facet_grid(treatment ~ growthShape) +
  #     geom_ribbon(aes(x = Time, y = NULL, colour = NULL,
  #                     ymin = p05*unitConvFact, ymax = p95*unitConvFact),
  #                 alpha = 0.25) +
  #     geom_ribbon(aes(x = Time, y = NULL, colour = NULL,
  #                     ymin = p25*unitConvFact, ymax = p75*unitConvFact),
  #                 alpha = 0.5) +
  #     geom_line(aes(x = Time, y = p50*unitConvFact),
  #               size  = 0.5, alpha = 1) +
  #     scale_colour_manual(name = "Carbon pool",
  #                         values = c("darkgreen","chocolate2", "coral4" )) +
  #     scale_fill_manual(name = "Carbon pool",
  #                       values = c("darkgreen","chocolate2", "coral4" )) +
  #     theme(plot.caption = element_text(size = rel(.75), hjust = 0),
  #           plot.subtitle = element_text(size = rel(.75))) +
  #     labs(title = paste0("Summary of aggregated pools\nSpecies: ", initC),
  #          subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
  #                            "\nMedians are represented with black lines",
  #                            "\n90% of values are comprised within lightly shaded areas",
  #                            "\n50% of values are comprised within darkly shaded areas."),
  #          x = "",
  #          y = expression(paste("tonnes C"," ha"^"-1","\n")),
  #          caption = paste0("ABio : Aboveground biomass stocks",
  #                           "\nBBio : Belowground (root) biomass stocks",
  #                           "\nTotalDOM : Total dead organic matter and soil stocks (DOM + SOM)"))
  #   
  #   fName <- paste0("pools_Summary_", s, "_", initC, ".png")
  #   
  #   png(filename= fName,
  #       width = 10, height = 6, units = "in", res = 600, pointsize=10)
  #   
  #   print(p)
  #   
  #   dev.off()
  # }
  
  
  
  # ###################################################################
  # ### Fluxes cell - level
  # 
  # ###################################################################
  # ###################################################################
  # ### very long, uncomment if update needed, else, read the csv
  # ###################################################################
  # log_Summary <- get(load(paste0("../outputCompiled/log_Summary_",
  #                                s, ".RData")))
  # ### pools
  # dfFluxes <- log_Summary %>%
  #   filter(variable %in% c("Turnover",  "NetGrowth", "NPP",
  #                            "Rh", "NEP")) 
  # 
  # dfFluxesPercentiles <- dfFluxes %>%
  #     group_by(areaName, treatment,
  #              initComm, growthShape, mortalityShape,
  #              Time,  variable) %>%
  #     summarise(p25 = quantile(C_gPerSqM, 0.25),
  #               p50 = quantile(C_gPerSqM, 0.5),
  #               p75 = quantile(C_gPerSqM, 0.75),
  #               p05 = quantile(C_gPerSqM, 0.05),
  #               p95 = quantile(C_gPerSqM, 0.95),
  #               meanVal = mean(C_gPerSqM),
  #               maxVal = max(C_gPerSqM),
  #               minVal = min(C_gPerSqM))
  # ###################################################################
  # ###################################################################
  # write.csv(dfFluxesPercentiles, file = paste0("fluxes_summaryPercentiles_", s, ".csv"), row.names = F)
  # ###################################################################
  # 
  dfFluxesPercentiles <- read.csv(paste0("../outputCompiled/fluxes_summaryPercentiles_", s, ".csv"))
  
  for(initC in unique(dfFluxesPercentiles$initComm)) {
    
    msLvls <- paste("mortalityShape =",
                    unique(dfFluxesPercentiles$mortalityShape))
    gsLvls <- paste("growthShape =",
                    unique(dfFluxesPercentiles$growthShape))
    df <- dfFluxesPercentiles %>%
      ungroup() %>%
      filter(initComm == initC,
             growthShape %in% gsSubset,
             mortalityShape %in% msSubset) %>%
      mutate(growthShape = factor(paste("growthShape =", growthShape),
                                  levels = gsLvls),
             mortalityShape = factor(paste("mortalityShape =", mortalityShape),
                                     levels = msLvls))
    
    x <- list()
    for (treat in c("CPRS", "CP")) {
      
      if(treat == "CPRS") {
        years <- c(425:600)
        zero <- 449
      } else {
        years <- c(440:615)
        zero <- 464
      }
      
      x[[treat]] <- df %>%
        filter(treatment == treat,
               Time %in% years) %>%
        mutate(zero = zero)
    }
    x <- do.call("rbind", x)
    
    for (treat in names(x)) {
      p <- ggplot(x, aes(x = Time - zero, #y = meanVal*unitConvFact,
                         colour = treatment,
                         fill = treatment,
                         linetype = mortalityShape,
                         group = (paste(treatment, growthShape, mortalityShape)))) +
        #theme_dark() +
        facet_grid(variable ~ growthShape,
                   scales = "free_y") +
        geom_line(#data = xPercent,
          aes(x = Time - zero, y = meanVal*unitConvFact),
          size  = 0.5,
          #colour = "lightblue",
          alpha = 1) +
        # geom_ribbon(#data = xPercent,
        #   aes(x = Time - zero, y = NULL, colour = NULL, 
        #       ymin = p05*unitConvFact, ymax = p95*unitConvFact),
        #   #fill = "lightblue",
        #   alpha = 0.25) +
        scale_colour_manual(name = "Treatment",
                            values = c(CP = "dodgerblue2", CPRS = "darkolivegreen")) +
        scale_fill_manual(name = "Treatment",
                          values = c(CP = "dodgerblue2", CPRS = "darkolivegreen")) +
        geom_hline(yintercept = 0, colour = "grey25", linetype = 2, size = 0.3) +
        geom_vline(xintercept = 0, colour = "grey25", linetype = 2, size = 0.3) +
        theme(plot.caption = element_text(size = rel(.5), hjust = 0),
              plot.subtitle = element_text(size = rel(.75))) +
        labs(title = paste0("Summary of ecosystem-level fluxes\nSpecies: ", initC),
             subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                               "\nMedians are represented with full lines",
                               "\n90% of values are comprised within shaded areas"),
             x = "Years\n(relative to harvest)\n",
             y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
             caption = paste0("Turnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
                              "\nNetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative as the stand ages and mortality outpaces growth.",
                              "\nNPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
                              "\nRh : Heterotrophic respiration. This is the sum of the 'To Air' fluxes through decomposition, not disturbance.",
                              "\nNEP : Net Ecosystem Productivity. NPP minus Rh."))
      
      fName <- paste0("fluxes_Summary_", s, "_", initC, ".png")
      png(filename= fName,
          width = 12, height = 10, units = "in", res = 600, pointsize=10)
      
      print(p)
      
      dev.off()
    }
    
  }
  
  
  ### Cumulative emissions / removals (ecosytem only)
  
  
  for(initC in unique(dfFluxesPercentiles$initComm)) {
    cols <- c("growthShape = 0.25" = "darkred",
              "growthShape = 0.5" = "darkblue",
              "growthShape = 1" = "darkgreen")
    
    df <- dfFluxesPercentiles %>%
      ungroup() %>%
      filter(initComm == initC,
             growthShape %in% gsSubset,
             mortalityShape %in% msSubset) %>%
      mutate(growthShape = factor(paste("growthShape =", growthShape),
                                  levels = gsLvls),
             mortalityShape = factor(paste("mortalityShape =", mortalityShape),
                                     levels = msLvls))
    
    
    x <- list()
    for (treat in c("CPRS", "CP")) {
      
      if(treat == "CPRS") {
        years <- c(449:750)
        zero <- 449
      } else {
        years <- c(464:765)
        zero <- 464
      }
      
      x[[treat]] <- df %>%
        filter(treatment == treat,
               Time %in% years) %>%
        group_by(areaName, treatment, initComm,
                 growthShape, mortalityShape,
                 variable) %>%
        arrange(Time) %>%
        mutate(zero = zero,
               cumulEmissions = -cumsum(p50),
               p05 = -cumsum(p05),
               p25 = -cumsum(p25),
               p50 = -cumsum(p50),
               p75 = -cumsum(p75),
               p95 = -cumsum(p95)) %>%
        ungroup()
    }
    x <- do.call("rbind", x)
  
    x <- filter(x, variable == "NEP") %>%
        mutate(variable = "Cumulative emissions/removals")
    
    p <- ggplot(x, aes(x = Time - zero, y = cumulEmissions*unitConvFact,
                   colour = growthShape,
                   fill = growthShape,
                   linetype = mortalityShape)) +
      facet_grid( ~treatment) +
  
      # geom_ribbon(aes(x = Time - zero, y = NULL, colour = NULL,
      #                 ymin = p25*unitConvFact, ymax = p75*unitConvFact),
      #             alpha = 0.1,
      #             show.legend = FALSE) +
      geom_line(aes(x = Time - zero, y = cumulEmissions*unitConvFact),
                size  = 0.5, alpha = 1) +
      geom_hline(yintercept = 0,
                 colour = "grey25", linetype = 2, size = 0.3) +
      theme(plot.caption = element_text(size = rel(.5), hjust = 0),
            plot.subtitle = element_text(size = rel(.75))) +
      scale_color_manual(name = "",
                         values = cols) +
      scale_fill_manual(values = cols) +
      scale_linetype_manual(name = "",
                            values = rev(c(1,2,3))) +
      labs(title = paste0("Cumulative emissions / removals (ecosystem only) \nSpecies: ", initC),
           subtitle = paste0("Single-cell simulations - ", nSims, " replicates",
                             "\nMedians are represented with full lines",
                             "\n50% of values are comprised within shaded areas"),
           x = "\nTime since harvest\n(years)",
           y = expression(paste("tonnes C"," ha"^"-1","\n")))
    
    fName <- paste0("cumulEmissions_", s, "_", initC, ".png")
    png(filename= fName,
        width = 10, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)
    dev.off()
  }
}

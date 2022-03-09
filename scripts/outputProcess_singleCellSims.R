###################################################################
###################################################################
### ForCS output processing for single cell simulations
rm(list = ls())
wwd <- "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/"
wwd <- paste(wwd, Sys.Date(), sep ="/")
dir.create(wwd)
setwd(wwd)

require(ggplot2)
require(dplyr)
require(tidyr)
require(doSNOW)
require(parallel)
require(foreach)
source("../scripts/fetchHarvestImplementationFnc.R")
unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
#####

simDir <- "D:/ForMont_validation/"
simName <- basename(simDir)


# for(s in simDir) {

s <- simDir

simInfo <- read.csv(paste(s, "simInfo.csv", sep ="/"),
                    colClasses = c(simID = "character"))

clusterN <- 6 ### seems optimal for hi I/O demand
###################################################################

 
################################################################################
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
################################################################################
### Biomass

log_BiomassC <- foreach(i = 1:nrow(simInfo), .combine = "rbind") %dopar% {
#for (i in 1:nrow(simInfo)) {
    require(data.table)
    require(dplyr)

    simID <- simInfo$simID[i]
    sDir <- paste(simDir, simID, sep = "/")
    area <- simInfo$areaName[i]
    landtype <- simInfo$landtypes[i]
    treatment <- simInfo$treatment[i]
    #growthShape <- simInfo$growthShape[i]
    #mortalityShape <- simInfo$mortalityShape[i]
    initComm <- simInfo$initComm[i]
    replicate <- simInfo$replicate[i]
    x <- read.csv(paste(sDir, "log_BiomassC.csv", sep = "/"))
    x <- x %>%
        dplyr::select(Time, species, Age, Wood, Leaf, CrsRoot, FineRoot)
    x <- data.frame(simID = simID,
                    areaName = area,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    #growthShape = growthShape,
                    #mortalityShape = mortalityShape,
                    replicate = replicate,
                    x)
    print(i)
    #log_BiomassC[[i]] <- x
    return(x)
}

# log_BiomassC <- melt(df, id.vars = c("simID", "areaName", "treatment", "initComm",
#                                      "landtype", "replicate", "Time", "species", "Age"))
stopCluster(cl)

log_BiomassC <- log_BiomassC %>%
    pivot_longer(c("Wood", "Leaf", "CrsRoot", "FineRoot"),
                 names_to = "pool", values_to = "biomassC_gPerSqM") %>%
    as.data.frame()

save(log_BiomassC, file = paste0("log_BiomassC_", simName, ".RData"))


################################################################################
### DOM & SOM
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#nrow(simInfo)

log_Pools <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)
    simID <- simInfo$simID[i]
    sDir <- paste(simDir, simID, sep = "/")
    area <- simInfo$areaName[i]
    landtype <- simInfo$landtypes[i]
    treatment <- simInfo$treatment[i]
    growthShape <- simInfo$growthShape[i]
    mortalityShape <- simInfo$mortalityShape[i]
    initComm <- simInfo$initComm[i]
    replicate <- simInfo$replicate[i]

    x <- read.csv(paste(sDir, "log_Pools.csv", sep = "/"))
    x <- x %>%
        dplyr::select(Time, species, VF_A, VF_B, Fast_A,
               MED, Slow_A, Slow_B, Sng_Stem, Sng_Oth)

    x <- data.frame(simID = simID,
                    areaName = area,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    # growthShape = growthShape,
                    # mortalityShape = mortalityShape,
                    replicate = replicate,
                    x)
    print(i)
    return(x)
}
stopCluster(cl)


log_Pools <- log_Pools %>%
    pivot_longer(c("VF_A", "VF_B", "Fast_A", "MED",
                   "Slow_A", "Slow_B", "Sng_Stem", "Sng_Oth" ),
                 names_to = "pool", values_to = "C_gPerSqM") %>%
    as.data.frame()

save(log_Pools, file = paste0("log_Pools_",simName, ".RData"))

# 
################################################################################
### Summary fluxes
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
#nrow(simInfo)

log_Summary <- foreach(i = 1:nrow(simInfo), .combine="rbind") %dopar% {
    require(dplyr)
    simID <- simInfo$simID[i]
    sDir <- paste(simDir, simID, sep = "/")
    area <- simInfo$areaName[i]
    landtype <- simInfo$landtypes[i]
    treatment <- simInfo$treatment[i]
    # growthShape <- simInfo$growthShape[i]
    # mortalityShape <- simInfo$mortalityShape[i]
    initComm <- simInfo$initComm[i]
    replicate <- simInfo$replicate[i]

    x <- read.csv(paste(sDir, "log_Summary.csv", sep = "/"))
    x <- x %>%
        dplyr::select(Time, ABio, BBio, TotalDOM,
               DelBio, Turnover, NetGrowth,	NPP, Rh, NEP, NBP)

    x <- data.frame(simID = simID,
                    areaName = area,
                    treatment = treatment,
                    initComm = initComm,
                    landtype = landtype,
                    # growthShape = growthShape,
                    # mortalityShape = mortalityShape,
                    replicate = replicate,
                    x)
    print(i)
    return(x)
}

stopCluster(cl)

log_Summary <- log_Summary %>%
    pivot_longer(c("ABio", "BBio", "TotalDOM", "DelBio",
                   "Turnover", "NetGrowth", "NPP", "Rh", "NEP", "NBP"),
                 names_to = "variable", values_to = "C_gPerSqM") %>%
    as.data.frame()

save(log_Summary, file = paste0("log_Summary_",simName, ".RData"))


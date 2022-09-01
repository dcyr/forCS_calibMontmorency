###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("\\\\", "/", home)
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")

dir.create(wwd)
setwd(wwd)

#####


### fetching outputs
a <- "ForMont"
sampleProp <- 0.005
simDir <- paste0("D:/ForCS - ForMont_2022-08-09/")#"#Montmorency-Hereford"#"D:/ForCS - "
simName <- gsub(paste0("ForCS - ", a, "_", collapse = "|"), "", basename(simDir))
#simDir <- paste0("D:/ForCS - Test/2020-06-11")#"#Montmorency-Hereford"#"D:/ForCS - "
simInfo <- read.csv(paste(simDir, "simInfo.csv", sep = "/"),
                    colClasses=c("simID"="character"))
x <- list.dirs(simDir, full.names = F, recursive = F)
###


###################################################################
require(data.table)
require(tidyverse)
require(raster)
require(doSNOW)
require(parallel)
require(foreach)


# ### hereford
# mgmtLevels <- c("1" = "Intensif",
#                 "3" = "Servitude",
#                 "4" = "Nouveau zonage",
#                 "2" = "Conservation")

# ### ForMont
# mgmtLevels <- c("1" = NA,
#                 "3" = NA,
#                 "4" = NA,
#                 "2" = NA)

source("../scripts/fetchHarvestImplementationFnc.R")


require(stringr)
clusterN <- 4
#######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

file.copy(paste(simDir, "simInfo.csv", sep = "/"),
          paste0("simInfo_", a, "_", simName, ".csv"), overwrite = T)

simIDs <- simInfo$simID
simIDs <- str_pad(simIDs, width = max(nchar(simIDs)),
                  side = "left", pad = "0")
dirIndex <- which(simIDs  %in% x &
                      simInfo$areaName == a)


out <- foreach(i = dirIndex)  %dopar% {#
  
    require(tidyverse)
    require(raster)
    require(reshape2)
    require(data.table)
    require(dplyr)
  
    output <- list()

    ### sim variables
    
    
    simID <-  simIDs[i]  
    sDir <-    paste(simDir, simID, sep ="/")
    areaName <- simInfo[i, "areaName"]
    scenario <- simInfo[i, "scenario"]
    mgmtScenario  <- simInfo[i, "mgmt"]
    mgmtScenarioName <- mgmtScenario
    harvest <- simInfo[i,"harvest"]
    replicate <- simInfo[i, "replicate"]
    
    ###
    sppLvls <- read.table(paste(sDir, "species.txt", sep = "/"),
                          skip = 1, comment.char = ">")[,1]
    if(is.factor(sppLvls)) {
        sppLvls <- levels(sppLvls)    
    }
    

    ### fetching landtypes
    landtypes <- raster(paste(sDir, "landtypes.tif", sep = "/"))
    landtypes_RAT <- read.table(paste(sDir, "landtypes.txt", sep = "/"),
                                skip = 1, comment.char = ">")
    landtypes_RAT <- landtypes_RAT[which(landtypes_RAT[,1] %in% c("yes", "y", "Yes", "Y")),]
    
    #studyArea <- raster(paste0("../inputsLandis/studyArea_", a, ".tif"))
    #landtypes[is.na(studyArea)] <- NA
    
    ## fetching lantypes values
    index <- which(!is.na(values(landtypes)))
    ltVal <- values(landtypes)[index]
    XY_lt <- rowColFromCell(landtypes, index)
    colnames(XY_lt) <- c("row", "column")
    XY_lt <- data.frame(XY_lt,
                        ecoregion = ltVal) #
    
    ##fetching harvest events
    if(harvest) {
        ### fetching mgmt areas, stand map and harvest implementation table
        mgmtAreas <- raster(paste(sDir, "mgmt-areas.tif", sep = "/"))
        standMap <- raster(paste(sDir, "stand-map.tif", sep = "/"))
        
        standtVals <- values(standMap)[index]
        
        XY <- XY_lt %>%
            mutate(mgmt = values(mgmtAreas)[index],
                   Stand = values(standMap)[index])
        
        x <- paste(sDir, "biomass-harvest.txt", sep = "/")
        harvImpl <- fetchHarvestImplementation(x)
        harv <- read.csv(paste(sDir, "harvest/log.csv", sep = "/"))
        
        valColIndex <- grep("BiomassHarvestedMg_", colnames(harv))
        valCol <- colnames(harv)[valColIndex]
        newColNames <- gsub("BiomassHarvestedMg_", "", valCol)
        
        
        harv <- XY %>%
            merge(harv, by = "Stand") %>%
            dplyr::select(Stand, row, column, ecoregion, mgmt,
                          Time, ManagementArea, Prescription, EventID, StandAge, StandRank,
                          all_of(valCol)) %>%
            pivot_longer(cols = starts_with("BiomassHarvestedMg_"),
                         names_to = "species",
                         names_prefix = "BiomassHarvestedMg_",
                         values_to = "BiomassHarvestedMg") %>%
            mutate(Prescription = gsub(" ", "", Prescription))
        
        harv <- data.frame(simID = simID,
                           areaName = areaName,
                           scenario = scenario,
                           mgmtScenario = mgmtScenario,
                           mgmtScenarioName = mgmtScenarioName,
                           replicate = replicate,
                           harv) %>%
            arrange(EventID)
        
        
        
        allEvents <- harv %>%
            dplyr::select(row, column, Time, EventID) %>%
            distinct()
        
        ### adding toFPS
        FluxBio <- fread(file = paste(sDir, "log_FluxBio.csv", sep = "/"))
        
        
        FluxBio <- FluxBio %>%
            merge(allEvents, all.x = F) %>%
          #group_by(Time, row, column, ecoregion) %>%
          group_by(Time, row, column, EventID, ecoregion, species) %>%
            summarize(MERCH_ToDOM = sum(MERCH_ToDOM),
                      FOL_ToDOM = sum(FOL_ToDOM),
                      OtherWoody_ToDOM =  sum(OtherWoody_ToDOM),
                      CrsRt_ToDOM =  sum(CrsRt_ToDOM),
                      FRt_ToDOM =  sum(FRt_ToDOM),
                      BioToFPS = sum(BioToFPS))
        
        output[["harv"]] <- harv %>%
          merge(FluxBio, all = T) %>%
          dplyr::select(!"BiomassHarvestedMg") %>%
          arrange(EventID)
        
        
        #######
        #####    selecting cells and timesteps to track
        events <- harv %>%
          filter(Time %in% c((min(harv$Time)+1):(max(harv$Time)))) %>%
          dplyr::select(row, column, Time, EventID, Prescription) %>%
          distinct() %>%
          slice_sample(prop = sampleProp)   
        
        ##fetching summary of C dynamics prior and after logging events (could eventually be implemented for all disturbances)
        for(e in 1:nrow(events)) {
          tmp <- events[e,]
          tmp <- data.frame(tmp[,c("row", "column", "EventID", "Prescription")],
                            Time = (tmp$Time-20):(tmp$Time+100),
                            eventTime = tmp$Time)
          if(e == 1) {
            eventDF <- tmp  
          } else {
            eventDF <- rbind(eventDF, tmp)
          }
        }
        eventDF <- data.frame(simID = simID,
                              areaName = areaName,
                              scenario = scenario,
                              mgmtScenario = mgmtScenario,
                              mgmtScenarioName = mgmtScenarioName,
                              replicate = replicate,
                              eventDF)
        
        
        ### fluxes and summary pools
        logSummary <- fread(file = paste(sDir, "log_Summary.csv", sep = "/"))
        outFluxes <- eventDF %>%
          merge(logSummary, all = F) 
       
        #### DOM pools (detailed)
        logPools <- fread(file = paste(sDir, "log_Pools.csv", sep = "/"))
        outDOM <- logPools %>%
          group_by(Time, row, column) %>%
          summarise(VF_A = sum(VF_A),
                    VF_B = sum(VF_B),
                    Fast_A = sum(Fast_A),
                    Fast_B = sum(Fast_B),
                    MED = sum(MED),
                    Slow_A = sum(Slow_A),
                    Slow_B = sum(Slow_B),
                    Sng_Stem = sum(Sng_Stem),
                    Sng_Oth = sum(Sng_Oth))
       
        outDOM <- eventDF %>%
          merge(outDOM, all = F)

        outFluxes <- eventDF %>%
          merge(outFluxes, all = F)
    
        output[["DOM"]] <- outDOM
        output[["fluxes"]] <- outFluxes
        
        
        
        
        ##fetching detailed AGB data prior and after logging events (could eventually be implemented for all disturbances)
        # ageClasses breaks
        breaks <- c(seq(0,120, by = 10),999)
        
        biomass <- fread(file = paste(sDir, "log_BiomassC.csv", sep = "/"))
        
        
        outBiomass <- eventDF %>%
          merge(biomass, all.y = F)
        ## 
        
        outBiomass[,"ageClass"] <- cut(outBiomass$Age, breaks)
        outBiomass <- outBiomass %>%
          #group_by(Time, row, column, EventID, eventTime, ecoregion) %>%
          group_by(simID, areaName, scenario, mgmtScenario, mgmtScenarioName, replicate,
                   row, column, EventID, eventTime, Prescription, Time, ecoregion, species, ageClass) %>%
          summarise(Wood = sum(Wood),
                    Leaf  = sum(Leaf ),
                    CrsRoot = sum(CrsRoot),
                    FineRoot = sum(FineRoot)) 
        
        output[["biomass"]] <- outBiomass
        return(output)
    }
}
stopCluster(cl)






### harvest log
harv <- list()
for(i in seq_along(out)) {
  harv[[i]] <- out[[i]][["harv"]]
}
harv <-do.call("rbind", harv) %>%
  arrange(simID, EventID, row, column, Time)
save(harv, file = paste0("standDynamics_", a, "_harvestLog_", simName,".RData"))

### fluxes
fluxes <- list()
for(i in seq_along(out)) {
  fluxes[[i]] <- out[[i]][["fluxes"]]
}
fluxes <-do.call("rbind", fluxes) %>%
  arrange(simID, EventID, row, column, Time)
save(fluxes, file = paste0("standDynamics_", a, "_fluxes_", simName, ".RData"))

### DOM pools
DOM <- list()
for(i in seq_along(out)) {
  DOM[[i]] <- out[[i]][["DOM"]]
}
DOM <-do.call("rbind", DOM) %>%
  arrange(simID, EventID, row, column, Time)
save(DOM, file = paste0("standDynamics_", a, "_DOM_", simName,".RData"))


### biomass
biomass <- list()
for(i in seq_along(out)) {
  biomass[[i]] <- out[[i]][["biomass"]]
}
biomass <-do.call("rbind", biomass) %>%
  arrange(simID, EventID, row, column, Time, species)
save(biomass, file = paste0("standDynamics_", a, "_biomass_", simName,".RData"))


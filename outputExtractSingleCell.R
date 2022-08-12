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
simDir <- paste0("D:/ForCS - ForMont_2022-08-09/")#"#Montmorency-Hereford"#"D:/ForCS - "
simName <- gsub("ForCS - ", "", basename(simDir))
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
          paste0("simInfo_", simName, ".csv"), overwrite = T)

simIDs <- simInfo$simID
simIDs <- str_pad(simIDs, width = max(nchar(simIDs)),
                  side = "left", pad = "0")
dirIndex <- which(simIDs  %in% x &
                      simInfo$areaName == a)


outputList <- foreach(i = dirIndex)  %dopar% {

    require(dplyr)
    require(raster)
    require(reshape2)
    require(data.table)
    require(dplyr)
    require(raster)

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
        harvLog <- read.csv(paste(sDir, "harvest/log.csv", sep = "/"))
        
        valColIndex <- grep("BiomassHarvestedMg_", colnames(harvLog))
        valCol <- colnames(harvLog)[valColIndex]
        newColNames <- gsub("BiomassHarvestedMg_", "", valCol)
        
        
        harvLog <- XY %>%
            merge(harvLog, by = "Stand") %>%
            dplyr::select(Stand, row, column, ecoregion, mgmt,
                          Time, ManagementArea, Prescription, EventID, StandAge, StandRank,
                          all_of(valCol)) %>%
            pivot_longer(cols = starts_with("BiomassHarvestedMg_"),
                         names_to = "species",
                         names_prefix = "BiomassHarvestedMg_",
                         values_to = "BiomassHarvestedMg") %>%
            mutate(Prescription = gsub(" ", "", Prescription))
        
        harvLog <- data.frame(simID = simID,
                          areaName = areaName,
                          scenario = scenario,
                          mgmtScenario = mgmtScenario,
                          mgmtScenarioName = mgmtScenarioName,
                          harvLog) %>%
            arrange(EventID)
        
        
        
        allEvents <- harvLog %>%
            dplyr::select(row, column, Time) %>%
            distinct()
        
        ### adding toFPS
        FluxBio <- fread(file = paste(sDir, "log_FluxBio.csv", sep = "/"))
        
        
        toFPS <- FluxBio %>%
            merge(allEvents, all.x = F) %>%
            group_by(Time, row, column, ecoregion, species) %>%
            summarize(AGBToDom = (sum(MERCH_ToDOM)+
                          sum(FOL_ToDOM) +
                          sum(OtherWoody_ToDOM))/10,
                BioToFPS_tonnesCTotal = sum(BioToFPS)/10) %>%
            mutate(BioToHell = AGBToDom + BioToFPS_tonnesCTotal)

        foo <- merge(harvLog, toFPS, all = T) %>%
            mutate(prop = BioToHell/BiomassHarvestedMg) 
    }
    
    
    
    #######
    #####    selecting cells and timesteps to track
    events <- harvLog %>%
        dplyr::select(row, column, Time) %>%
        distinct() %>%
        slice_sample(prop = 0.01)
    
    
    
    ##fetching summary of C dynamics prior and after logging events (could eventually be implemented for all disturbances)
    for(e in 1:nrow(events)) {
        tmp <- events[e,]
        tmp <- data.frame(tmp[,c("row", "column")],
                           Time = (tmp$Time-20):(tmp$Time+100),
                          eventTime = tmp$Time)
        if(e == 1) {
            outFluxes <- tmp  
        } else {
            outFluxes <- rbind(outFluxes, tmp)
        }
    }
    
    logSummary <- fread(file = paste(sDir, "log_Summary.csv", sep = "/"))
    outFluxes <- outFluxes %>%
        merge(logSummary, all.y = F) %>%
        arrange(eventTime, row, column, Time)

  
     ## fetching detailed AGB sequences
    tmp <- outFluxes %>%
        dplyr::select(row, column, Time, eventTime, ecoregion)
  
    
    
    agb <- fread(file = paste(sDir, "log_BiomassC.csv", sep = "/"))
    
    ## 
    breaks <- c(seq(0,120, by = 10),999)
    
    
    out <- tmp %>%
        merge(agb, all.y = F)
    
    out[,"ageClass"] <- cut(out$Age, breaks)
    out <- out %>%
        group_by(Time, eventTime, row, column, ecoregion,  species, ageClass) %>%
        summarise(Wood = sum(Wood),
                  Leaf  = sum(Leaf ),
                  CrsRoot = sum(CrsRoot),
                  FineRoot = sum(FineRoot)) %>%
        arrange(eventTime, row, column, Time)
    
    
   
    ### adding to FPS fluxes
   
    
    
    foo <- out %>%
        merge(FluxBio, all.x = T) %>%
        arrange(eventTime, row, column, Time)
    
    
    
    head()
        ## focusing on targetted area
        FluxBio <- merge(FluxBio, xy, all.y = F)
        ### summarizing FPS
        toFPS <- FluxBio %>%
            group_by(Time, species) %>%
            summarize(BioToFPS_tonnesCTotal = round(prod(res(mgmtAreas))/10000 * sum(BioToFPS)/100,2)) %>%
            mutate(areaManagedTotal_ha = totalArea)
        
        areaHarvested <-  FluxBio %>%
            filter(BioToFPS > 0) %>%
            distinct(Time, row, column) %>%
            group_by(Time) %>%
            summarise(areaHarvestedTotal_ha = prod(res(mgmtAreas))/10000 * n())

        if(nrow(areaHarvested) > 0) {
            toFPS <- merge(toFPS, areaHarvested)
        } else {
            toFPS[,"areaHarvestedTotal_ha"] <- 0
        }
        
        
        
        toFPS$species <- factor(toFPS$species, levels = sppLvls)
        
        toFPS <- data.frame(simID = as.character(simID),
                            areaName = areaName,
                            scenario = scenario,
                            mgmtScenario  = mgmtScenario,
                            mgmtScenarioName = mgmtScenarioName,
                            replicate = replicate,
                            toFPS)
        
        output[["FPS"]] <- toFPS
        rm(toFPS)
        
    }
    
    print(paste('Done with simulation', simID))
    return(output)
    
}
stopCluster(cl)

if("summary"  %in% logs ) {
    ### summary
    outputSummary <- list()
    for(i in seq_along(outputList)) {
        outputSummary[[i]] <- outputList[[i]][["summary"]]
        
    }
    outputSummary <-do.call("rbind", outputSummary)
    save(outputSummary, file = paste0("output_summary_", simName, ".RData"))
}

if("agbAgeClasses"  %in% logs ) {
    ### agbAgeClasses
    output_agbAgeClasses <- list()
    for(i in seq_along(outputList)) {
        output_agbAgeClasses[[i]] <- outputList[[i]][["agbAgeClasses"]]
        
    }
    output_agbAgeClasses <- do.call("rbind", output_agbAgeClasses)
    save(output_agbAgeClasses, file = paste0("output_bio_", simName, ".RData"))
}
if("FPS"  %in% logs ) {
    ### summary
    outputSummary <- list()
    for(i in seq_along(outputList)) {
        outputSummary[[i]] <- outputList[[i]][["FPS"]]
        
    }
    outputSummary <-do.call("rbind", outputSummary)
    write.csv(outputSummary, file = paste0("output_BioToFPS_", simName, ".csv"), row.names = F)
    #save(outputSummary, file = paste0("output_BioToFPS_", a, ".RData"))
}








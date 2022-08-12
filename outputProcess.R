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


###################################################################
require(data.table)
require(dplyr)
require(raster)
require(doSNOW)
require(parallel)
require(foreach)

logs <- c("summary",
          "agbAgeClasses",
          "agbTotal",
          "ageMax",
          "FPS"
          ) #"FPS" ,"summary", "agbAgeClasses", "agbAgeClasses", "agbTotal","ageMax",

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

if("summary" %in% logs) {
    source("../scripts/fetchHarvestImplementationFnc.R")
}

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
                        ltID = ltVal) #
    
    
    if("summary" %in% logs) {
        if(harvest) {
            ### fetching mgmt areas and harvest implementation table
            mgmtAreas <- raster(paste(sDir, "mgmt-areas.tif", sep = "/"))
            x <- paste(sDir, "biomass-harvest.txt", sep = "/")
            harvImpl <- fetchHarvestImplementation(x) 
        } else {
            mgmtAreas <- landtypes
            mgmtAreas[!is.na(landtypes)] <- 1
        }

    }


    if("agbAgeClasses" %in% logs) {
        #############
        ## computing landtype area size
        areaSize_lt <- data.frame(freq(landtypes))
        areaSize_lt[,"area_ha"] <- areaSize_lt$count * (prod(res(landtypes))/10000)
        areaSize_lt <- data.frame(ltID = areaSize_lt$value,
                                  ltArea_ha = areaSize_lt$area_ha)
        areaSize_lt <- areaSize_lt[complete.cases(areaSize_lt),]

        if(!("summary" %in% logs)) {
            XY <- XY_lt
        }
    }

    if("summary" %in% logs) {
        #############
        ## computing mgmg area size
        areaSize_mgmt <- data.frame(freq(mgmtAreas))
        areaSize_mgmt[,"area_ha"] <- areaSize_mgmt$count * (prod(res(mgmtAreas))/10000)
        areaSize_mgmt <- data.frame(mgmtID = areaSize_mgmt$value,
                                    mgmtArea_ha = areaSize_mgmt$area_ha)
        areaSize_mgmt <- areaSize_mgmt[complete.cases(areaSize_mgmt),]

        #############
        ## XY df
        #############
        ## fetching management areas values
        index <- which(!is.na(values(mgmtAreas)))
        mgmtVal <- values(mgmtAreas)[index]
        XY_mgmt <- rowColFromCell(mgmtAreas, index)
        colnames(XY_mgmt) <- c("row", "column")
        XY_mgmt <- data.frame(XY_mgmt,
                              mgmtID = mgmtVal) #
        if(!("agbAgeClasses" %in% logs)) {
            XY <- XY_mgmt
        }
    }


    if("summary" %in% logs &
       "agbAgeClasses" %in% logs) {
        XY <- merge(XY_lt, XY_mgmt, all.x = T)
    }


    if("summary" %in% logs) {
        ####
        logSummary <- fread(file = paste(sDir, "log_Summary.csv", sep = "/"))
        ### subsetting outputs
        df <- merge(XY, logSummary)
        ### summarizing by management area
        dfSummary <- df %>%
            group_by(mgmtID, Time) %>%
            summarise(simID = simID,
                      areaName = areaName,
                      scenario = scenario,
                      mgmtScenario  = mgmtScenario,
                      mgmtScenarioName  = mgmtScenarioName,
                      replicate = replicate,
                      ABio = mean(ABio),
                      BBio = mean(BBio),
                      TotalDOM = mean(TotalDOM),
                      DelBio = mean(DelBio),
                      Turnover = mean(Turnover),
                      NetGrowth = mean(NetGrowth),
                      NPP = mean(NPP),
                      Rh = mean(Rh),
                      NEP = mean(NEP),
                      NBP = mean(NBP)) %>%
            merge(areaSize_mgmt)

        ### tidying up...
        dfSummary <- melt(dfSummary, id.vars = c("simID",
                                                 "areaName", "scenario", "mgmtScenario", "mgmtScenarioName",
                                                 "replicate", "Time", "mgmtID", "mgmtArea_ha")) %>%
            arrange(simID, Time, mgmtID, variable)

        output[["summary"]] <- dfSummary

        rm("logSummary", "df")

    }

    if("agbAgeClasses"  %in% logs |
       "agbTotal"  %in% logs |
       "ageMax"  %in% logs) {
        ####
        agb <- fread(file = paste(sDir, "log_BiomassC.csv", sep = "/"))
        #### focusing on study area
        XY_studyArea <- XY[!is.na(XY$mgmtID),]
        agb <- agb %>%
            merge(XY_studyArea, all.x = F)

        if("agbAgeClasses"  %in% logs) {
            # first reduce the size of the table (before merging the XY df)
            breaks <- c(seq(0,120, by = 20),999)
            agb[,"ageClass"] <- cut(agb$Age, breaks)


            ###
            ltVals <- landtypes_RAT$V2
            
            ###
            zeroPadDF <- expand.grid(species = unique(agb$species),
                                     ageClass = unique(agb$ageClass),
                                     landtype = ltVals,
                                     Time = unique(agb$Time),
                                     agb_tonnesTotal = NA)

            ### summarizing by landtype
            
            
            agbSummary <- agb %>%
                mutate(landtype = ltID,
                       agb_tonnes = prod(res(landtypes))/10000*
                           2*(Wood + Leaf)/100) %>%
                group_by(landtype, Time, species, ageClass) %>%
                summarise(agb_tonnesTotal = sum(agb_tonnes))


            ### summarizing AGB
            agbSummary <- agbSummary %>%
                merge(zeroPadDF,
                      by = c("species", "ageClass","landtype", "Time"),
                      all.y = T) %>%
                merge(areaSize_lt,
                      by.x = "landtype", by.y = "ltID") %>%
                mutate(agb_tonnesTotal = ifelse(is.na(agb_tonnesTotal.x), 0, agb_tonnesTotal.x),
                       agb_tonnesPerHa = round(agb_tonnesTotal / ltArea_ha, 2))


            ### tidying up
            agbSummary <- data.frame(simID = as.character(simID),
                                     areaName = areaName,
                                     scenario = scenario,
                                     mgmtScenario  = mgmtScenario,
                                     mgmtScenarioName  = mgmtScenarioName,
                                     replicate = replicate,
                                     agbSummary[, c("Time", "landtype", "species", "ageClass",
                                                    "agb_tonnesTotal", "ltArea_ha", "agb_tonnesPerHa")])


            colnames(agbSummary)[which(colnames(agbSummary) == "ltArea_ha")] <- "landtypeArea_ha"

            output[["agbAgeClasses"]] <- agbSummary


        }

        if("agbTotal"  %in% logs) {
            ### summarizing by landtype
            agbTotal <- agb %>%
                mutate(agb_tonnesPerHa = 2*(Wood + Leaf)/100) %>%
                group_by(row, column, Time, species) %>%
                summarise(agb_tonnesPerHa = round(sum(agb_tonnesPerHa), 2))

            agbTotal$species <- factor(agbTotal$species, levels = sppLvls)

            agbTotal <- data.frame(simID = as.character(simID),
                                   areaName = areaName,
                                   scenario = scenario,
                                   mgmtScenario  = mgmtScenario,
                                   mgmtScenarioName  = mgmtScenarioName,
                                   replicate = replicate,
                                   agbTotal)

            save(agbTotal, file = paste0("agbTotal_", simName, "_", simID, ".RData"))
            rm(agbTotal)
        }

        if("ageMax" %in% logs) {
            ageMax <- agb %>%
                group_by(row, column, Time) %>%
                summarise(ageMax = max(Age))

            ageMax <- data.frame(simID = as.character(simID),
                                 areaName = areaName,
                                 scenario = scenario,
                                 mgmtScenario  = mgmtScenario,
                                 replicate = replicate,
                                 ageMax)
            save(ageMax, file = paste0("ageMax_", simName, "_", simID, ".RData"))
            rm(ageMax)
        }

        rm(agb)
        
    }
    
    if("FPS"  %in% logs
       & harvest) {
        ### fetching targetted mgmt-areas
        ### fetching landtypes
        mgmtAreas <- raster(paste(sDir, "mgmt-areas.tif", sep = "/"))
        # if(a %in% c("Hereford", "ForMont")) {
        #     mgmtAreas <- mgmtAreas >= 10000  
        # } else {
        #     mgmtAreas[!is.na(mgmtAreas)] <- 1
        # }
        
        
        r <- mgmtAreas
        r[] <- 1:ncell(r)
        xy <- as.data.frame(zonal(mgmtAreas, r))
        xy <- cbind(xy,
                    row = rowFromCell(mgmtAreas,xy$zone),
                    column = colFromCell(mgmtAreas,xy$zone)) %>%
            filter(mean == 1)
        xy <- xy[, c("row", "column")]
        totalArea <- as.data.frame(zonal(mgmtAreas, mgmtAreas, sum))
        totalArea <- filter(totalArea, zone == 1)[,2] * prod(res(mgmtAreas))/10000
        
        ####
        FluxBio <- fread(file = paste(sDir, "log_FluxBio.csv", sep = "/"))
        ## correcting error in file format
        #cNames <- c(colnames(FluxBio)[-1], "V1")
        #colnames(FluxBio) <- cNames
        #FluxBio <- FluxBio[,c(1:17)]
        
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








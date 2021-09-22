################################################################################
################################################################################
### Generates simulation packages for LANDIS-II 
### Dominic Cyr
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep ="/"))
################################################################################
################################################################################
wwd <- paste(getwd(), Sys.Date(), sep = "/")
#wwd <- paste(getwd(), "test", sep = "/")
dir.create(wwd)
setwd(wwd)
#############
require(stringr)
require(raster)

#spinUp <- T

inputDir <- "../inputsLandis/"
timestep <- 1
simDuration <- 1000
simArea <- "ForMont"
harvest <- "biomass"
plant <- TRUE
noRecruitment <- F
#inputs <- list.files(inputDir)



### loading landtypes
landtypes <- raster(paste0(inputDir, "/landtypes_", simArea, ".tif"))
studyArea <- raster("../inputsLandis/studyArea_ForMont.tif")


# ### focus on studyArea
# ltSA <- resample(landtypes, studyArea, "ngb")
# ltSA[is.na(studyArea)] <- NA
# table(values(ltSA))



##############################

# speciesList <- list(bor = paste(c("ACER.SAH 1", "FAGU.GRA 1",
#                                   "POPU.TRE 1", "ABIE.BAL 1",
#                                   "BETU.ALL 1", "BETU.PAP 1",
#                                   "ACER.RUB 1"), collapse = "\n"))
speciesList <- list(ABIE.BAL = "ABIE.BAL 1",#,
                    PICE.GLA = "PICE.GLA 1")
                    # mix = paste(c("ABIE.BAL 1", 
                    #               "PICE.GLA 1",
                    #               "BETU.PAP 1"),
                                  # collapse = "\n")
    #)
 



timestep <- 1
expDesign <- list(area = simArea,
                  #landtypes = c("221"),#
                  landtypes = c("220","221", "222", "223"),
                  #landtypes = c("425_1", "425_2", "425_3", "425_4", "425_5"), ## Papineau
                  treatment = list("CPRS" = seq(from = 150, to = 450, by = 100),#,
                                   "CP" = seq(from = 150, to = 480, by = 35)),
                                   
                  nrep = 5)

simInfo <- expand.grid(areaName = expDesign$area,
                       landtypes = expDesign$landtypes,
                       treatment = names(expDesign$treatment),
                       growthShape =  c(0.1, 0.25, 0.5, 0.75, 0.8, 0.85, 0.875,
                                        seq(from = 0.9, to = 1, by = 0.01)),
                       initComm = names(speciesList),
                       replicate = 1:expDesign$nrep,
                       noRecruitment = noRecruitment)

sID <- (1:nrow(simInfo))-1
simInfo <- data.frame(simID = str_pad(sID, nchar(max(sID)),
                                      pad = "0"),
                      simInfo)

# require(parallel)
# require(doSNOW)
# n <- floor(detectCores() * .25)
# 
# # #######
# cl = makeCluster(n, outfile = "") ## 
# registerDoSNOW(cl)
#foreach(i = 1:nrow(simInfo)) %dopar% {

for (i in 1:nrow(simInfo)) {
    require(stringr)
    require(raster)
    require(tidyverse)
    dt <- "INT4S" ### raster encoding type
    
    landtypesTableFile <- paste0(inputDir, "/landtypes_", simArea, ".txt")
    landtypes_RAT <- read.table(landtypesTableFile, skip = 1,comment.char = ">")
    colnames(landtypes_RAT) <- c("Active", "Code", "Name", "Description")
    
    landtypeName <- as.character(simInfo[i, "landtypes"])
    landtypeCode <- landtypes_RAT[match(landtypeName, landtypes_RAT$Name), "Code"]
    
    simID <- as.character(simInfo[i,"simID"])
    areaName <- as.character(simInfo[i,"areaName"])
    replicate <- as.character(simInfo[i,"replicate"])
    initComm <- speciesList[[as.character(simInfo[i,"initComm"])]]
    gs <- simInfo[i,"growthShape"]  
    
    prescript <- expDesign$treatment[simInfo[i, "treatment"]]
    
    
    dir.create(simID)
    
    ### creating raster files and associated attribute tables
    InitCommVal <- 1
    r <- raster(matrix(InitCommVal))
    
    rLandtype <- r
    rLandtype[] <- landtypeCode
    
    writeRaster(rLandtype,
                filename = paste(simID, "landtypes.tif", sep = "/"),
                overwrite = T, datatype = dt)
    writeRaster(r,
                filename = paste(simID, "initial-communities.tif", sep = "/"),
                overwrite = T, datatype = dt)
    
    ### storing landtypes for later
    lt <- read.table(landtypesTableFile, skip = 1)
    lt <- lt[lt[,1] %in% ("yes"),][,2]
    
    
    file.copy(landtypesTableFile, paste(simID, "landtypes.txt", sep ="/"), overwrite = T)
    sink(file = paste(simID, "initial-communities.txt", sep = "/"))
    
        cat('LandisData "Initial Communities"\n\n')
        cat(paste("MapCode", InitCommVal, "\n"))
        cat(initComm)
        cat("\n")
        
    sink()
    
    
    
    ###############################################
    ### scenario.txt
    
    x <- paste0(inputDir, "/scenario_singleCellSims.txt")
    
    x <- readLines(x)
    # duration
    index <- grep("Duration", x)
    x[3] <- paste("Duration", simDuration)
    
    sink(file = paste(simID, "scenario.txt", sep = "/"))
    cat(paste(x, collapse = "\n"))
    sink()
    
    ### species.txt
    sppFile <- paste0(inputDir, "/species_",
                      areaName, ".txt")
    
    file.copy(sppFile,
              paste0(simID, "/species.txt"),
              overwrite = T)
    
    sppFull <- read.table(sppFile, skip = 1, comment.char = ">")[,1]
    sppFull <- as.character(sppFull)
    
    ### readme
    write.table(t(simInfo[i,]), file = paste0(simID, "/README.txt"),
                quote = F, col.names = F)
    

    
    ##############################################
    ### Succession extension
    
    # ForC-succession
   
    x <- paste0(paste0(inputDir, "/forCS-input_",
                       areaName, "_baseline.txt"))
    fName <-  paste0(simID, "/forCS-input.txt")

        
    x <- readLines(x)
    
    
    #### manipulating growth shape parameters (and potentially other parameters)
    index <- grep("SpeciesParameters|DOMPools", x)
    spp <- as.character(gsub("[0-9]|\n", "", initComm))
    spp <- unlist(strsplit(spp, " "))
    
        
        
    tmp <- x[index[1]:(index[2]-1)]
    headerIndex <- grep("SpeciesParameters|>>", tmp)
    tableIndex <- min(headerIndex):max(headerIndex)
    tableIndex <- tableIndex[!tableIndex %in% headerIndex]
    tailPart <- tmp[headerIndex[headerIndex>max(tableIndex)]]
    header <-  tmp[headerIndex[headerIndex<min(tableIndex)]]
    
    tmp <- tmp[tableIndex]
    writeLines(tmp,"tmp.txt")
    
    tmp <- read.table("tmp.txt")
    unlink("tmp.txt")
    sppIndex <- which(tmp[,1] %in% spp) 
    tmp[sppIndex, 8] <- gs
    unlink
    
    
    
    ##### writing to file
    sink(file = fName)
    
    cat(paste(x[1:(index[1]-1)], collapse = "\n"))
    cat("\n")
    writeLines(header)
    cat("\n")
    sink()
    
    write.table(tmp, file = fName,
                append = T,
                row.names = F,
                col.names = F,
                sep = "\t",
                quote = F,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\n")
    
    sink(file = fName,
          append = T)
    
    writeLines(tailPart)
    cat("\n")
    sink()
    
    if(noRecruitment) {
        #### manipulating growth shape parameters (and potentially other parameters)
        indexRecruit <- grep("EstablishProbabilities|RootDynamics", x)
        
        newSEP <- expand.grid(Yr = 0, Ecoregion = lt, Spp = sppFull, Prob = 0) %>%
            arrange(Yr, Ecoregion, Spp)
        
        sink(file = fName,
             append = T)
        
        
        
        cat(paste(x[index[2]:indexRecruit[1]], collapse = "\n"))
        cat("\n")
        cat(">>  Yr   Ecoregion   Spp         Prob\n")
        cat(">> ------------------------- ---------------\n")
        cat("\n")
        sink()
        
        
        write.table(newSEP, file = fName,
                    append = T,
                    row.names = F,
                    col.names = F,
                    sep = "\t",
                    quote = F,
                    #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                    eol = "\n")
        sink(file = fName,
             append = T)
        cat("\n")
        cat(paste(x[indexRecruit[2]:length(x)], collapse = "\n"))
        cat("\n")
        sink()
        
    } else {
        sink(file = fName,
             append = T)
        cat(paste(x[index[2]:length(x)], collapse = "\n"))
        cat("\n")
        sink() 
    }
    
    
    
    
    
   
    
    
    # cat("\n")
    # cat(">> How Frequently the four different output files should be printed.  (intervals in years)\n")
    # cat(">> Output interval\n")
    # cat(">>  Biomass\tDOM_Pools\tFluxes\tSummary\n")
    # cat(">>  ----------------------------------------------------------------\n")
    #
    # cat(paste(c(rep(timestep, 4), "\n\n"), collapse = "\t"))
    #
    # cat(paste(x[index[2]:length(x)], collapse = "\n"))
    # sink()
    #
    # file.copy(paste0(inputDir, "/ForC-succession_",
    #                  areaName, ".txt"),
    #           paste0(simID, "/ForC-succession.txt"),
    #           overwrite = T)
    
    # Climate inputs
    file.copy(paste0(inputDir, "/forCS-climate_",
                     areaName, "_baseline.txt"),
              paste0(simID, "/forCS-climate.txt"),
              overwrite = T)
    # DM inputs
    file.copy(paste0(inputDir, "/ForCS_DM_",
                     areaName, "_baseline.txt"),
              paste0(simID, "/ForCS_DM.txt"),
              overwrite = T)
    
    # Biomass succession
    
    # other ?
    
    ###############################################
    ### Disturbances
    
    ### Harvesting
    # stand map
    writeRaster(r, paste0(simID, "/stand-map.tif"),
              overwrite = T, datatype = dt)
    
    # management areas
    writeRaster(r, paste0(simID, "/mgmt-areas.tif"),
                overwrite = T, datatype = dt)
    
    
    if(harvest == "base") {
        ###############################################
        ### base-harvest.txt
        x <- paste0(inputDir, "/base-harvest_",
                    areaName, "_generic.txt")
        
        x <- readLines(x)
        fName <-  paste(simID, "base-harvest.txt", sep = "/")
        index <- grep("HarvestImplementations|PrescriptionMaps", x)
    }
    
    if(harvest == "biomass") {
        ###############################################
        ### base-harvest.txt
        x <- paste0(inputDir, "/biomass-harvest_",
                    areaName, "_generic.txt")
        
        x <- readLines(x)
        fName <-  paste(simID, "biomass-harvest.txt", sep = "/")
    }
    
    index <- grep("HarvestImplementations|PrescriptionMaps", x)
    

    harvImpl <- data.frame(MgmtArea = values(r),
                           Prescription = names(prescript),
                           HarvestArea = "100%",
                           BeginTime = prescript[[1]],
                           EndTime = prescript[[1]])
    
    
    
    
    prescriptBlock <- x[1:index[1]]
    
    
    if(plant) {### currently work when there's only one prescription where to include planting
        prescriptIndex <- which(trimws(prescriptBlock) == paste("Prescription", names(prescript)))
        
        gaps <- which(nchar(prescriptBlock)==0)
        
        gapIndex <- min(gaps[gaps>prescriptIndex])
        
        ## insert planting instruction
        prescriptBlockNew <- prescriptBlock[1:gapIndex-1]  
        prescriptBlockNew <-c(prescriptBlockNew, paste(c("Plant", spp), collapse = " "))
        
        prescriptBlock <- c(prescriptBlockNew, prescriptBlock[gapIndex:length(prescriptBlock)])  
            
    }  
    
    sink(file = fName)
    
    cat(paste(prescriptBlock, collapse = "\n"))
    
    cat(paste("\n>>", paste(colnames(harvImpl), collapse = "\t"), "\n\n"))
    
    sink()
    write.table(harvImpl, file  = fName, append = T,
                col.names = F, row.names = F, quote = F)
    
    sink(file = fName, append = T)
    
    cat(paste(x[index[2]:length(x)], collapse = "\n"))
    
    sink()  

}

write.csv(simInfo, file = "simInfo.csv", row.names = F)
### simPilot.R
file.copy("../scripts/simPilot_singleCellSims.R",
          "simPilot.R",
          overwrite = T)

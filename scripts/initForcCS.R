################################################################################
################################################################################
### Preparation Forest Carbon Succession inputs
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
require(raster)
require(sp)
require(rgdal)
require(ggplot2)
require(broom)
require(dplyr)
require(maptools)
require(RCurl)


################################################################################
### script path
scriptPath <- paste("../scripts", sep = "/")
### input paths (CBM)
inputPathGIS <- paste(home, "Sync/Travail/ECCC/GIS", sep = "/")
inputPathAIDB <- paste(home, "Sync/Travail/ECCC/CBM/AIDB", sep = "/")
inputPathSPU <- paste(home, "Sync/Travail/ECCC/CBM/spatial", sep = "/")
### input path (LANDIS)
inputPathLandis <- "../inputsLandis"

aidbURL <- inputPathAIDB
spuURL <- inputPathSPU
################################################################################
#### Sourcing scripts
source(paste(scriptPath, "CBMtoLANDIS_fnc.R", sep = "/"), encoding = "Windows-1252")
source(paste(scriptPath, "initForCS_fnc.R", sep = "/"), encoding = "Windows-1252")

################################################################################
landisInputs <- list.files(inputPathLandis)
### experiment specifics
scenario <- c("RCP45", "RCP85")#, 
area <-  "ForMont"#c("Hereford", "ForMont")#"Maskinonge"
t0 <- 2020
inputOffset <- -20
version <- "3.1"
spinup <- F
climate <- T
allometry = T
#interpolate

################################################################################
# might want to create loops here, or a function
#a <- area
for(a in area) {
        
    ### fetch species.txt
    species <- landisInputs[grep("species", landisInputs)]
    species <- species[grep(a, species)]
    species <- species[!grepl("Copy", species)]
    species <- read.table(paste(inputPathLandis, species, sep = "/"),
                          skip = 1, comment.char = ">")
    ### fetching landtypes
    landtypes <- landisInputs[grep("landtypes", landisInputs)]
    landtypes <- landtypes[grep(a, landtypes)]
    landtypes_AT <- landtypes[grep("txt", landtypes)]
    landtypes_AT <- read.table(paste(inputPathLandis, landtypes_AT, sep = "/"),
                               skip = 1, comment.char = ">")
    landtypes <- landtypes[grep("tif", landtypes)]
    index <- grep("cropped", landtypes)
    if(length(index)>0) {
        landtypes <- landtypes[-index]
    }
    
    landtypes <- raster(paste(inputPathLandis, landtypes, sep = "/"))
    
    landtypeNames <- landtypes_AT[which(landtypes_AT$V1 == "yes"), "V3"]
    
    #s <- "baseline"
    for(s in scenario) {
        
        ### fetching succession extensions inputs and template
        bsMainInput <- paste0("../inputsLandis/biomass-succession-main-inputs_",
                              a,"_", s, ".txt")
        bsDynInput <-  paste0("../inputsLandis/biomass-succession-dynamic-inputs_",
                              a, "_", s, "_BiasCorrected.txt")
        forCSInput <- paste0("../inputsLandis/forCS-input_template.txt")
            

        ### Preparing 'forCS-input.txt' and 'forCS-climate.txt'
        initForCS(forCSInput, bsMainInput, bsDynInput, landtypes, landtypes_AT,
                  version = version,
                  spinup = spinup,
                  climate = climate,
                  allometry = allometry,
                  t0 = t0,
                  scenario = s)
   }
}


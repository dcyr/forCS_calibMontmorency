################################################################################
################################################################################
### Some code to produce a area extent within which we fetch NFI plots for 
### study area calibration
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(rgeos)
require(dplyr)
areas <- "ForMont" #c("Hereford", "Maskinonge")
bufferMeters <- 200000
### input path (LANDIS)
inputPathGIS <- paste(home, "Sync/Travail/ECCC/GIS", sep = "/")
inputPathLandis <- "../inputsLandis"
source("../scripts/gdal_polygonizeR.R")

ecoregions <- get(load(paste(inputPathGIS, "CanEcologicalFramework/ecoregions.RData", sep = "/")))

for (a in areas) {
    landtypes <- raster(paste0(inputPathLandis, "/landtypes_", a, ".tif"))
    eco <- spTransform(ecoregions, CRSobj = crs(landtypes))
    
    ##
    r <- extend(landtypes, extent(landtypes)+(bufferMeters*2))
    ## buffer du territoire à l'étude
    b <- r
    b <- buffer(b, width = bufferMeters,
                doEdge=T)
    # create polygon from buffer
    
    ############################################################################
    ## extract ecoregions that intersect study area
    
    ## rasterize polygons (long...)
    ecoC <- crop(eco, landtypes)
    ecoL <- rasterize(ecoC, landtypes)
    ecoL[is.na(landtypes)] <- NA
    RAT <- levels(ecoL)[[1]]
    vals <- unique(RAT[match(values(ecoL), RAT$ID), "ECOREGION"])
    vals <- vals[!is.na(vals)]
    ## 
    ecoC <- crop(eco, b)
    ecoB <- rasterize(ecoC, b)
    ecoB[is.na(b)] <- NA
    
    ## remove ecoregions that do not intersect from raster
    index  <- which(levels(ecoB)[[1]][,"ECOREGION"] %in% vals)
    ecoB[!(ecoB %in% index)] <- NA
    # plot(ecoB)
    # plot(is.na(ecoB))
    lvls <- levels(ecoB)[[1]]
    
    bPoly <- gdal_polygonizeR(ecoB)
    bPoly <- merge(bPoly, lvls[,c("ID", "REGION_NAM", "REGION_NOM")],
                   by.x = "DN", by.y = "ID")
    save(bPoly, file = paste0("ecoregion_Buffer", bufferMeters/1000, "km_", a, ".RData"))
    writeOGR(bPoly, ".", paste0("ecoregion_Buffer", bufferMeters/1000, "km_", a),
             driver="ESRI Shapefile", overwrite_layer = T)
}


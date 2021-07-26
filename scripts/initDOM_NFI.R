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
areas <- c("Hereford", "Maskinonge", "ForMont")
bufferMeters <- 200000
### input path (LANDIS)
inputPathGIS <- paste(home, "Sync/Travail/ECCC/GIS", sep = "/")
inputPathLandis <- "../inputsLandis"
inputPathNFI <- paste(home, "Sync/Travail/ECCC/NFI", sep = "/")
source("../scripts/gdal_polygonizeR.R")

NFI_sites <- read.csv(paste(inputPathNFI, "NFI_all_gp_site_info_approx_loc.csv", sep = "/"))
NFI_soils <- read.csv(paste(inputPathNFI, "NFI_all_gp_soil_site_info.csv", sep = "/"))

### convert UTM to lat long


# 1 "Very Fast Aboveground"
# 2 "Very Fast Belowground"
# 3 "Fast Aboveground"
# 4 "Fast Belowground"
# 5 "Medium"
# 6 "Slow Aboveground"
# 7 "Slow Belowground"
# 8 "Stem Snag"
# 9 "Other Snag"
# 10 "Extra pool"


################################################################################
################################################################################
#### a function to extract DOM pools 

extract_CBM_DOMpools <- function(df) {
    domPools <- list("Fast_A" = list(names = c("plotbio_fwd",
                                               "plotbio_swd" ),
                                     id = "3",
                                     convFact = 50),
                     "MED" = list(names = "plotbio_wd",
                                  id = "5",
                                  convFact = 50),
                     "Slow_A" = list(names = "cc_for_floor_8mm",
                                     id = "6",
                                     convFact = 100),
                     "Slow_B" = list(names = c("cc_min0_15",
                                               "cc_min15_35",
                                               "cc_min35_55"),
                                     id = "7",
                                     convFact = 100),
                     "Sng_Stem" = list(names = "plotbio_lgtr_dead",
                                       id = "8",
                                       convFact = 50),
                     "Sng_Oth" = list(names = c("plotbio_stump",
                                                "plotbio_smtr_dead"),
                                      id = "9",
                                      convFact = 50))
    ## replace -7 by NAs
    for (j in 1:ncol(df)) {
        y <- df[,j]
        index <- which(y == -7)
        y[index] <- NA
        df[,j] <- y
    }
    
    domSummary <- list()
    for (d in seq_along(domPools)) {
        domID <- names(domPools[d])
        domCol <- domPools[[d]]$names
        convFactor <- domPools[[d]]$convFact
        x <- apply(as.data.frame(df[,domCol]), 1, function(x)
            sum(x, na.rm = ifelse(domID %in% c("Sng_Oth","Sng_Stem", "MED"), F, T))*convFactor)
        if(domID %in% c("Fast_A", "Slow_B", "Slow_A")) {
            x[x==0] <- NA
        }
        # if(domID %in% c("Slow_B", "Slow_A", "plotbio_wd")) {
        #     x <- apply(as.data.frame(df[,domCol]), 1, function(x) sum(x, na.rm = T)*convFactor)
        #     
        # }
        domSummary[[domID]] <- x
    }
    
    domSummary <- do.call("cbind", domSummary)
    return(domSummary)
}

################################################################################
################################################################################
#### creating SpatialPointsDataframe from NFI plots

utmZone <- NFI_sites$utm_zone
u <- unique(utmZone)
u <- u[order(u)]
nfi <- list()
for(i in seq_along(u)) {
    z <- u[i]
    df <- NFI_sites %>%
        filter(utm_zone == z) %>%
        merge(NFI_soils, all.x = T, all.y = F)
    
    
    df <- data.frame(df, extract_CBM_DOMpools(df))
    
    xy <- SpatialPoints(data.frame(x = df$utm_e,
                                   y = df$utm_n),
                        proj4string=CRS(paste0("+proj=utm +zone=", z, "+datum=NAD83")))
    
    
    xy <- SpatialPointsDataFrame(xy, df)
    xy <- spTransform(xy, CRS("+init=epsg:4269"))
    nfi[[i]] <- xy
}

nfi_DOM <- do.call("rbind", nfi)

################################################################################
################################################################################
#### computing summaries by ecozones

domSummary <- list()
for(a in areas) {

    ecoBuffer <- get(load(paste0("../gis/ecoregion_Buffer200km_", a, ".RData")))
    nfi_DOM_buffer <- spTransform(nfi_DOM, CRSobj = crs(ecoBuffer))
    ecoDF <- over(nfi_DOM_buffer, ecoBuffer)
    
    eco <- unique(ecoDF$REGION_NAM)
    eco <- eco[!is.na(eco)]
    domSummary[[a]] <- list()
    for (e in eco) {
        domSummary[[a]][[e]] <- list()
        index <- which(ecoDF$REGION_NAM == e)
        df <- as.data.frame(nfi_DOM_buffer[index,])
        cNames <- c("Fast_A", "MED", "Slow_A", "Slow_B", "Sng_Stem", "Sng_Oth")
        for (d in seq_along(cNames)) {
            convFact <- 
            if(cNames[d] %in% colnames(df)) {
                domVal <- df[,cNames[d]]    
                x <- mean(domVal, na.rm = T)
                domSummary[[a]][[e]][[cNames[d]]] <- x
            }
        }
    }
}
# 
# foo <- numeric()
# for(a in areas) {
#     foo <- append(foo, sum(unlist(domSummary[[a]])))
# }

################################################################################
################################################################################
#### Assing DOM values based on ForCS spinup

for(a in areas) {
    DOMinitPools <- read.csv(paste0("../inputsLandis/DOM-initPools_", a, ".csv"))
    
    DOMinitProp <- DOMinitPools %>%
        group_by(landtype) %>%
        summarize(DOMtotalLt = sum(amountAtT0)) %>%
        ungroup() %>%
        merge(DOMinitPools) %>%
        group_by(landtype, DOMtotalLt, spp) %>%
        summarize(DOMtotalSpp = sum(amountAtT0)) %>%
        ungroup() %>%
        merge(DOMinitPools) %>%
        mutate(propSpp = DOMtotalSpp/DOMtotalLt,
               propPool = amountAtT0/DOMtotalSpp)
    
    
    ### assign an ecoregion to every landtype
    landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
    ecoBuffer <- get(load(paste0("../gis/ecoregion_Buffer200km_", a, ".RData")))
    ecoBuffer <- spTransform(ecoBuffer, CRSobj = crs(landtypes))
    ecoRAT <- levels(ecoBufferR)
    ecoBufferR <- rasterize(ecoBuffer, landtypes)
    ecoRAT <- levels(ecoBufferR)[[1]]
    
    ecoFreqTable <- table(values(ecoBufferR), values(landtypes))
    
    ecoMajID <- as.numeric(rownames(ecoFreqTable))[apply(ecoFreqTable, 2, which.max)]
    ecoMajID <- data.frame(landtype = colnames(ecoFreqTable),
                      ecoregion = ecoRAT[match(ecoMajID, ecoRAT$ID), "REGION_NAM"])
    
    DOMinitPoolsNew <- list()
    for(i in unique(DOMinitProp$landtype)) {
        e <- as.character(ecoMajID[which(ecoMajID$landtype == i), "ecoregion"])
        DOMnfi <- domSummary[[a]][[e]]
        DOMnfi <- data.frame(nfi_ref = as.numeric(DOMnfi),
                             DOMName = names(DOMnfi))
        
        index <- which(DOMinitProp$landtype == i)
        DOMinitPoolsNew[[as.character(i)]] <- DOMinitProp[index,] %>%
            merge(DOMnfi, all.x = T) %>%
            # mutate(amountAtT0New = propSpp * nfi_ref,
            #        amountAtT0New = ifelse(is.na(amountAtT0New), 0, amountAtT0New))
            mutate(amountAtT0 = propSpp * nfi_ref,
                   amountAtT0 = round(ifelse(is.na(amountAtT0), 0, amountAtT0))) %>%
            select(landtype, spp, poolID, DOMName, amountAtT0) %>%
            arrange(landtype, spp, poolID)
    }
    
    DOMinitPoolsNew <- do.call("rbind", DOMinitPoolsNew)
    write.csv(DOMinitPoolsNew, file = paste0("DOM-initPools_", a, ".csv"), row.names = F)
    #merge(DOMinitPools, DOMinitPoolsNew, by = c("landtype", "spp", "poolID"))
}

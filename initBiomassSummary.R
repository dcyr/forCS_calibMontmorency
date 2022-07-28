rm(list=ls())
require(raster)
require(dplyr)
require(RCurl)
#setwd("/home/dcyr/Dropbox/LANDIS-II_IA_SCF/InitBiomassData")
inputFolder <- "D:/BiomassKNN/"

setwd("C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)


# ##

### computing summary statistics from rasters of individual species
# quantiles to compute
q <- c(0.001, 0.01, 0.1,
       0.25, 0.50, 0.75,
       0.9, 0.95, 0.99, 0.999)

###
files <- list.files(inputFolder, full.names = T)



sppFiles <- files[grep("NFI_MODIS250m_2011_kNN_Species_", files)]

###
info <- strsplit(basename(sppFiles), "_")
year <- as.character(lapply(info, function(x) x[3]))
###
spp <- as.character(lapply(info, function(x) paste(x[6], x[7], sep = ".")))
spp <- toupper(spp)

### study area
studyArea <- raster("../inputsLandis/studyArea_ForMont.tif")
x <- raster(sppFiles[1])
saProj <- projectRaster(studyArea,
                        res = res(x),
                        crs = crs(x), method="ngb")  
saProj <- trim(saProj)

### total AGB 
agbDensity <- raster(files[grep("NFI_MODIS250m_2011_kNN_Structure_Biomass_TotalLiveAboveGround_v1.tif", files)])
agbDensity<- resample(AGBtotal, saProj, method="ngb")  
agbDensity[is.na(saProj)] <- NA
### prop forest
vegTreedProp <-  raster(files[grep("NFI_MODIS250m_2011_kNN_LandCover_VegTreed_v1.tif", files)])
vegTreedProp<- resample(vegTreedProp, saProj, method="ngb")  
vegTreedProp[is.na(saProj)] <- NA


### crop to study area

r <- list()

for(i in seq_along(spp)) { #
    x <- raster(sppFiles[i])
    x <- resample(x, saProj, method="ngb")  
    x[is.na(saProj)] <- NA

    r[[spp[i]]] <- x
   
    print(i)
}

r <- stack(r)
## correcting for forested area
sppAGB_prop <- r/100
agbTotal <- agbDensity
agbSpp <- sppAGB_prop * agbTotal
names(agbSpp) <- spp
#foo <- r * 100/vegTreedProp


# min(apply(values(r), 1, sum, na.rm = F), na.rm = T)
# quantile(apply(values(r), 1, function(x) sum(x, na.rm = F)), q, na.rm =T)

#totalArea <- unique(apply(rVals, 2, function(x) sum(!is.na(x))) * prod(res(r))/10000)

### summarizing
roundN <- 2
rVals <- values(agbSpp)
cellArea <- prod(res(r))/10000



sppSummary <- as.data.frame(t(apply(rVals, 2, function(x) quantile(x, q, na.rm = T))))

sppSummary[, "agbMax_tonnesPerHa"] <- apply(rVals, 2, function(x) max(x, na.rm = T))
sppSummary[, "agbMean_tonnesPerHa"] <- apply(rVals, 2, function(x) mean(x, na.rm = T))
sppSummary <- sppSummary[order(sppSummary$agbMean_tonnesPerHa, decreasing = T),]
#sppSummary <- round(sppSummary, roundN)


AGBTotalSummary <- apply(rVals, 1, sum)
AGBTotalSummary <- c(quantile(AGBTotalSummary, q,  na.rm = T),
                     agbMax_tonnesPerHa = max(AGBTotalSummary2, na.rm = T),
                     agbMean_tonnesPerHa = mean(AGBTotalSummary, na.rm = T),
                     cumulProp = 1)



sppSummary[,"cumulProp"] <- cumsum(sppSummary$agbMean_tonnesPerHa)/AGBTotalSummary["agbMean_tonnesPerHa"]




AGBTotalSummary <- as.data.frame(t(AGBTotalSummary))
rownames(AGBTotalSummary) <- "total"


AGBTotalSummary
sppSummary

write.csv(rbind(AGBTotalSummary, sppSummary),
          file = "biomassSummary.csv", row.names = T)
          

                                 


# t(data.frame(averageAGB_tonnesPerHa = 
# rSums <- apply(rVals, 2, sum, na.rm = T)
# 
# #for (a in unique(areas)) {
#     a <- "LSJ"
#     
#     index <- grep(a, areas)
#     summaryStats <- data.frame(species = c(spp[index], "Total"))
#     
#     meanAGBiomass_TonsPerHa <- maxAGBiomass_TonsPerHa <- numeric()
#     quantiles <- matrix(NA, nrow = nrow(summaryStats), ncol = length(q))
#     colnames(quantiles) <- paste0("quantile", q)
#     ## loading landtypes
#     readURL <- "https://github.com/dcyr/LANDIS-II_IA_generalUseFiles/raw/master/LandisInputs/"
#     tmpFile <- tempfile()
#     url <- paste(readURL, a, "/landtypes_", a, ".tif", sep="")
#     download.file(url, tmpFile, method="wget")
#     landtypes <- raster(tmpFile)
#     ##
#     sppStack <- stack(files[index])
#     sppStack[is.na(landtypes)] <- NA
#     sppStack <- stack(sppStack, sum(sppStack))
#     ## considering only active landtypes
#     names(sppStack) <- c(spp[index], "Total")
#     
#     for (i in seq_along(1:nlayers(sppStack))) {
#         r <- sppStack[[i]]
#         ## using only a subset
#         # r[is.na(BSESubsetRaster)] <- NA
#         rValues <- values(r)
#         meanAGBiomass_TonsPerHa <-  append(meanAGBiomass_TonsPerHa, mean(rValues, na.rm=TRUE))
#         maxAGBiomass_TonsPerHa <-  append(maxAGBiomass_TonsPerHa, max(rValues, na.rm=TRUE))
#         quantiles[i, ] <- quantile(rValues, q, na.rm=TRUE)
#         print(paste(a, names(r)))
#     }
#     
#     summaryStats <- data.frame(meanAGBiomass_TonsPerHa,
#                                maxAGBiomass_TonsPerHa,
#                                quantiles)
#     summaryStats <- round(summaryStats, 3)
#     summaryStats <- data.frame(species = names(sppStack),
#                                summaryStats)
#     
#     
#     
#     write.csv(summaryStats, file = paste0("initBiomassSummaryStats_", a, ".csv"), row.names = F)
# #}
# 
# 
# 
# #}

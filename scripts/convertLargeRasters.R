require(raster)
standMap <- raster("C:/Users/cyrdo/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/inputsLandis/stand-map_ForMont.tif")
nCells <- sum(values(!is.na(standMap)))
#
standMap[!is.na(standMap)] <- 1:nCells
#"INT4S", "INT4U", "FLT4S"
writeRaster(standMap,
            file = "stand-map_ForMont.tif",
            datatype = "INT4S",
            NAflag = 0,
            overwrite = T)


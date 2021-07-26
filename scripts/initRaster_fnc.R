x <- list.files(inputPathLandis)
x <- x[grep("initial-com", x)]
x <- x[grep(".tif", x)]

for (i in seq_along(x)) {
    r <- raster(paste(inputPathLandis, x[i], sep = "/"))
    writeRaster(r, file = x[i], NAflag = 0, datatype="INT4S", overwrite=TRUE)
}

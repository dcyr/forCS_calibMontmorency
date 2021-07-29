### A function that reads the LANDIS-II 'base-harvest.txt' input file
### and returns the harvest implementation table as a data.frame

fetchHarvestImplementation <- function(x) { #function(x, prescripLvls) {
    
    harvestImpTmp <- readLines(x)
    index <- grep("HarvestImplementations|PrescriptionMaps", harvestImpTmp)
    harvestImpTmp <- harvestImpTmp[index[1]:(index[2]-1)]
    harvestImpTmp <- harvestImpTmp[-which(substr(harvestImpTmp, 1,1) == ">")][-1]
    harvestImpTmp <- harvestImpTmp[-which(nchar(harvestImpTmp) == 0)]
    #HarvestImpTmp <-  HarvestImpTmp[grep(paste(prescriptLvls, collapse = "|"), HarvestImpTmp)]
    #### putting into a nice data frame
    for (i in seq_along(harvestImpTmp)) {
        x <- strsplit(harvestImpTmp[[i]], "\\t")[[1]]
        x <- x[nchar(x)!=0]
        
        #prescripLvls <- 
        
        tmp <- data.frame(mgmtArea = as.numeric(x[1]),
                          prescription = factor(x[2]),
                          harvestAreaProp_target = as.numeric(gsub("%", "", x[3]))/100)
                          #beginTime = x[4])
        if(i == 1) {
            harvestImplementations <- tmp
        } else {
            harvestImplementations <- rbind(harvestImplementations, tmp)
        }
    }
    return(harvestImplementations)
}


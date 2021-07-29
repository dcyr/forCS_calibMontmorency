
################################################################################
################################################################################
### Preparation of base-harvest files
### Dominic Cyr
#############

# input <- paste(inputPathLandis, "biomass-harvest_Hereford_1.txt", sep = "/")
# timestepOutput = 1
# harvestExtensionOut <- "Base Harvest" ## either 'Base Harvest' or ,Biomass Harvest'
# writeToFile = "base-harvest.txt"
# 
# initBaseHarvest(input = paste(inputPathLandis, "biomass-harvest_Hereford_1.txt", sep = "/"),
#                 writeToFile = T)

initBaseHarvest <-  function(input,
                             timestepOutput = 1,
                             writeToFile = T) {


    
    if(is.logical(writeToFile)) {
        if(writeToFile) {
            outFile <- "base-harvest.txt"
        } else {
            outFile <- NULL
        }
    } else {
        outFile <- writeToFile
        writeToFile <- T
    }

    valuesSingleAll <- c("Timestep", "ManagementAreas", "Stands")

    x <- readLines(input)

    valueSingleFlags <- grep(paste(valuesSingleAll, collapse = "|"), x)
    prescriptFlags <- grep("Prescription ", x)
    HarvestImplFlags <- grep("HarvestImplementations", x)
    
    
    flagsAll <- c(valueSingleFlags, prescriptFlags, HarvestImplFlags)
    flagsAll <- flagsAll[order(flagsAll)]
    
    ### single value parameters
    out <- list()
    for (i in seq_along(valuesSingleAll)) {
        v <- valuesSingleAll[i]
        
        index <- valueSingleFlags[i]
        
        tmp <-  x[valueSingleFlags[i]]
        tmp <- gsub(v, "", tmp)
        # remove anything after comment char
        tmp <- strsplit(tmp, ">")[[1]]
        # remove spaces
        tmp <- gsub(" ", "", tmp)
        # replace backslash with forward slash
        tmp <- gsub("([\\])","/", tmp)
        # remove quotes
        tmp <- gsub('"',"", tmp)
        # remove tabs
        tmp <- gsub("([\t])","", tmp)
        # convert to numeric, if possible
        tmp2 <- suppressWarnings(as.numeric(tmp))
        if(!sum(is.na(tmp2)) > 0) {
            tmp <- tmp2
        }
        out[[v]] <- tmp
    }
    
    
    
    ############################################################################
    #### updating values 
    
    ### Timestep and HarvestImplementations
    tmp <- x[HarvestImplFlags:length(x)][-1]
    ## parsing table
    tmp <- strsplit(tmp, " ")
    tmp <- lapply(tmp, function(x) x[which(nchar(x) > 0)])
    # remove empty lines
    index <- which(as.numeric(lapply(tmp, length)) == 0)
    if(length(index)>0) {
        tmp <- tmp[-index]   
    }

    ### removing output file path typically contained in biomass-harvest input files
    index <- which(as.logical(lapply(tmp, function(x) grepl("PrescriptionMaps|BiomassMaps|EventLog|SummaryLog", x[[1]]))))
    if(length(index) > 0) {
        tmp <- tmp[-index]    
    }
    
    
    ### adapt code if there are some begin/end times
    # as.numeric(lapply(tmp, length))
    tmp <- lapply(tmp, function(x) data.frame(mgmtArea = x[[1]],
                                              prescription = x[[2]],
                                              areaToHarvest = x[[3]]))
    out[["HarvestImplementations"]] <- do.call("rbind", tmp)
    
    if(timestepOutput != out$Timestep) {
        tmp <- out[["HarvestImplementations"]]
        areaToHarvest <- as.character(out[["HarvestImplementations"]][,3])
        areaToHarvest <- as.numeric(gsub("%", "", areaToHarvest))
        areaToHarvest <- timestepOutput * areaToHarvest/out$Timestep
        
        out$Timestep <- timestepOutput
        out[["HarvestImplementations"]][,3] <- paste0(areaToHarvest, "%")
    }
    
    # File names
    out$ManagementAreas <- "mgmt-areas.tif"
    out$Stands <- "stand-map.tif"
    
    
    
    #### writing everything to file
    
    sink(outFile)
    cat('LandisData "Base Harvest"\n')
    cat("\n")
    cat(paste("Timestep", out$Timestep))
    cat("\n")
    cat("\n")
    cat(paste("ManagementAreas", out$ManagementAreas))
    cat("\n")
    cat(paste("Stands", out$Stands, "\n"))
    cat("\n")
    
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>> This 'Base Harvest' input file was automatically generated and is based on 'Biomass Harvest' input file:\n")
    cat(paste0(">>>> '",input, "'\n"))
    cat(">>>> Proportion of stand removed (1/n) and harvest implementation (% area to harvest per timestep) should be doubled checked prior to simulation\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat("\n")
    
    ### prescriptions - Convert targetted proportion to 1/n
    #### prescription
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>> Prescriptions\n")
    cat(">>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")

    flag <- F
    for(i in min(prescriptFlags):(HarvestImplFlags-1)) {
    
        tmp <- x[i]
        if(nchar(tmp) == 0)
            next
        if(grepl("Prescription", tmp)) {
            flag <- F
            cat("\n")
        }
        if(grepl("CohortsRemoved", tmp)) {
            flag <- T
        }
        if(grepl("Plant", tmp)) {
            flag <- F
        }
        if(flag) {
            if(substr(tmp, 1, 1) != ">") {
                if(grepl("%",tmp)) {
                    sp <- strsplit(tmp, " ")[[1]][1]
                    target <-  strsplit(tmp, " ")[[1]][2]
                    ageClass <- strsplit(target, "\\(")[[1]][1]
                    prop <- strsplit(target, "\\(")[[1]][2]
                    prop <-as.numeric(gsub("%)", "", prop))
                    if(prop == 100) {
                        tmp <- paste(sp, ageClass)
                    } else {
                        if(prop>50) {
                            warning(paste0("Don't know what to do when we want to harvest > 50% but less than 100%\n",
                                           "Set harvest prescription to 100% removal..."))
                            tmp <- paste(sp, ageClass)
                        } else {
                            tmp <- paste0(sp, " 1/", floor(100/prop))    
                        }
                        
                    }
                    
                } 
            }
        }
        
        if(grepl("Prescription", tmp)) {
            cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
        }
        if(grepl("MinimumTimeSinceLastHarvest|StandRanking|SiteSelection|CohortsRemoved|Plant", tmp)) {
            cat(">>>>>>>>>>>>>>>>>>>>\n")
        }
        cat(paste0(tmp, "\n"))
    }
    
    cat("\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat("HarvestImplementations\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(paste0(">>\t", paste(colnames(out$HarvestImplementations), collapse = "\t"), "\n"))
    sink()
    
    sink(outFile, append = T)
    write.table(out$HarvestImplementations, file = outFile,
                append = T,
                sep = "\t",
                quote = F,
                col.names = F,
                row.names = F,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\n") #default line endings on windows system.)
    
    cat("\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>> Output files\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    
    
    cat("PrescriptionMaps harvest/prescripts-{timestep}.tif\n")
    cat("EventLog harvest/log.csv\n")
    cat("SummaryLog harvest/summarylog.csv\n")
    cat(">>>>>>>>>>>>>>>>>>>>\n")
    cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
    
    sink() 
}

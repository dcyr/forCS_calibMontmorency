################################################################################
################################################################################
#### This function takes LANDIS species codes (as defined in "vegCodes.csv"),
#### and returns CBM specie (as defined in AIDB)

# landisURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master"
# aidbURL = "https://raw.githubusercontent.com/dcyr/CBM_genUseFiles/master/AIDB"
# spuURL = "https://raw.githubusercontent.com/dcyr/CBM_genUseFiles/master/spatial"


sppConvert <- function(spp, inputCode, ### where "inputCode" is either "CBM" or "LANDIS"
                           exceptions = NULL, ## currently a placeholder for when it might be preferable to assign another species
                           landisURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master",
                           aidbURL = aidbURL) {  
    
    ### convert to character if that's not already the case
    library (readr)  
    spp <- as.character(spp)
    
    ### fetching species lookup tables
    vegCodesURL <- paste(landisURL, "vegCodes.csv", sep="/")
    #vegCodes <- read.csv(text = getURL(vegCodesURL))  
    vegCodes <- as.data.frame(read_csv(url(vegCodesURL)))
    
    if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
      #tblSpeciesTypeDefault <- read.csv(text = getURL(paste(aidbURL,
      #                                            "tblSpeciesTypeDefault.txt", sep="/"))) 
      tblSpeciesTypeDefault <- read_csv(url(paste(aidbURL,
                                                            "tblSpeciesTypeDefault.txt", sep="/")))
      
    } else {
      tblSpeciesTypeDefault <- read.csv(paste(aidbURL,"tblSpeciesTypeDefault.txt", sep="/")) 
    }
    ### species names
    sppAIDB <- as.character(tblSpeciesTypeDefault$SpeciesTypeName)
    codeAIDB <-  as.character(tblSpeciesTypeDefault$CanFI_Code)
    sppCommonName <- as.character(vegCodes$commonName)
    sppLANDIS <- as.character(vegCodes$LandisCode)
    
    if(inputCode == "LANDIS") {
        index <- match(spp, sppLANDIS)
        if(sum(is.na(index))>0) {
            stop(paste("Some input code(s) couldn be found in reference list provided in",
                       vegCodesURL))
        }
        #
        sppCode <- as.character(vegCodes[index, "Code"])
        sppOut <- sppAIDB[match(sppCode, codeAIDB)]
        
        if(!is.null(exceptions)) {# enforce exceptions
            exceptionIndex <- which(spp %in% names(exceptions))
            for (i in exceptionIndex) {
                sppOut[i] <- exceptions[[spp[i]]]
            }
        }
        
        # checking for remaining NA's
        indexNA <- which(is.na(sppOut))
        if(length(indexNA)>0) {
            stop(paste("A match for the following species code couldn't be found:",
                   spp[indexNA]))
        } else {
            print("A perfect match was found for all input species code")
            names(sppOut) <- spp
            return(sppOut)
        }
    }

    if(inputCode == "CBM") {
        index <- match(spp, sppAIDB)
        #
        sppCode <- tblSpeciesTypeDefault[index, "CanFI_Code"]#"Code"]
        sppOut <- as.character(vegCodes[match(sppCode, vegCodes$Code), "LandisCode"])
        
        
        if(!is.null(exceptions)) {# enforce exceptions
            exceptionIndex <- which(spp %in% names(exceptions))
            for (i in exceptionIndex) {
                sppOut[i] <- exceptions[[spp[i]]]
            }
        }
        
        # checking for remaining NA's
        indexNA <- which(is.na(sppOut))
        if(length(indexNA)>0) {
            stop(paste("A match for the following species code couldn't be found:",
                       spp[indexNA]))
        } else {
            print("A perfect match was found for all input species code")
            names(sppOut) <- spp
            return(sppOut)
        }
    }
}

################################################################################
################################################################################
#### This function modifies the SoilSpinUp section so that it activate the appropriate flag
#### and sets the initial DOM pools to zero
#### (Could eventually offer more functionalities)

soilSpinUp <- function(forCS, 
                       soilSpinUp = T,
                       tolerance = 1, 
                       maxIter = 20) {
  
  forCS$SoilSpinUp$table[,1] <- as.numeric(soilSpinUp)
  forCS$SoilSpinUp$table[,2] <- tolerance
  forCS$SoilSpinUp$table[,3] <- maxIter

  return(forCS)
}



################################################################################
################################################################################
#### This function takes LANDIS landtypes (ecoregions) and returns CBM
#### spatial units ID (SPUs)

spuFetch <- function(landtypes, landtypes_AT,
                     onlyActive = T,  ### returns values only for active landtypes
                     rule = "majority",
                     aidbURL = aidbURL,
                     spuURL = spuURL) { ### just a placeholder at the moment
    
    
    print("Matching LANDIS landtypes with CBM SPUID")
    ### LANDIS inputs
    isActive <- landtypes_AT$V1 %in% c("yes", "y", "Yes", "Y", T)
    ltNames <- as.character(landtypes_AT$V3)
    ltID <- landtypes_AT$V2
    
    if(onlyActive) {
        ltNames <- ltNames[isActive]
        ltID <- ltID[isActive]
    }
    
    if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
      ### CBM spu table
      tblSPUDefault <- read.csv(text = getURL(paste(aidbURL,
                                                    "tblSPUDefault.txt", sep="/")))
    } else {
      ### CBM spu table
      tblSPUDefault <- read.csv(paste(aidbURL,
                                      "tblSPUDefault.txt", sep="/")) 
    }
    
    if(strsplit(spuURL, "/")[[1]][1] %in% c("https:", "http:")) {
      ### spatial unit raster
      tmpFile <- paste0(tempfile(), ".tif")
      url <- paste(spuURL, "spuR.tif", sep="/")
      download.file(url, tmpFile, method="curl")
      spuR <- raster(tmpFile)
      ### spatial unit attribute table
      url <- paste(spuURL, "spuR_AT.csv", sep="/")
      spuR_AT <- read.csv(text = getURL(url))  
    } else {
      spuR <- raster(paste(spuURL, "spuR.tif", sep = "/"))
      spuR_AT <- read.csv(paste(spuURL, "spuR_AT.csv", sep = "/"))  
    }
  
    ### matching landtypes with CBM spatial units
    # reproject and crop
    spuR <- projectRaster(spuR, landtypes,  method="ngb")
    # matching cells with table lines
    index <- match(values(spuR), spuR_AT$ID)
    outputVals <- c("SPUID", "EcoBoundar", "ProvinceID") ## might have to modify some code if it's anything else
    
    spuVals <- spuR_AT[index, c("EcoBoundar", "ProvinceID")]

    spuVals <- merge(tblSPUDefault, spuVals,
                 by.x = c("AdminBoundaryID", "EcoBoundaryID"),
                 by.y = c("ProvinceID", "EcoBoundar"),
                 all.y = T)
    
    freqTable <- table(spuVals$SPUID, values(landtypes))
    if(rule == "majority") {
        SPUID <- apply(freqTable, 2, function(x) as.numeric(names(x)[which.max(x)]))
    }
    return(SPUID)
}


################################################################################
################################################################################
#### ## create a list of tables for all Biomass Succession main inputs

landisInputFetch <- function(input, type) { ## 'type' is one of 'BiomassSuccession' or 'ForCS'
    require(stringr)
    if(type == "BSMain") {
        valuesSingleAll <- c("Timestep", "SeedingAlgorithm",
                             "InitialCommunities",
                             "InitialCommunitiesMap",
                             "CalibrateMode",
                             "SpinupMortalityFraction",
                             "DynamicInputFile",
                             "AgeOnlyDisturbances:BiomassParameters")
        tablesAll <- c("MinRelativeBiomass", "SufficientLight",
                       "SpeciesParameters",
                       "EcoregionParameters")
    }
    
    if(type == "BSDynamics") {
        x <- read.table(input, skip = 1, 
                        comment.char = ">")
        colnames(x) <- c("year", "landtype", "species",
                         "probEst", "maxANPP", "maxB")
        return(x)
    }
    
    
    if(type == "ForCS") {
        valuesSingleAll <- c("Timestep", "SeedingAlgorithm", "ForCSClimateFile",
                             "InitialCommunities", "InitialCommunitiesMap")
        
        
        tablesAll <- c("ForCSOutput", "SoilSpinUp", "AvailableLightBiomass",
                       "LightEstablishmentTable", "SpeciesParameters",
                       "DOMPools", "EcoSppDOMParameters", "ForCSProportions",
                       "DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                       "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass",
                       "ANPPTimeSeries", "MaxBiomassTimeSeries",
                       "EstablishProbabilities", "RootDynamics",
                       "SnagData")
        # if(as.numeric(version)>=3.1) {
        #   
        #   tablesDM <- c("DisturbFireTransferDOM", "DisturbOtherTransferDOM",
        #                 "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass")
        #   
        #   valuesSingleAll <- c(valuesSingleAll,	"DisturbanceMatrixFile")
        #   
        #   tablesAll <- tablesAll[!tablesAll %in% tablesDM]
        # }
          
      
    }
    
    
    
    x <- readLines(input)
    valueHeaderFlags <- grep(paste(valuesSingleAll, collapse = "|"), x)
    tableHeaderFlags <- grep(paste(tablesAll, collapse = "|"), x)
    flagsAll <- c(valueHeaderFlags, tableHeaderFlags)
    flagsAll <- flagsAll[order(flagsAll)]
    
    out <- list()
    for (i in seq_along(valuesSingleAll)) {
        v <- valuesSingleAll[i]
        index <- valueHeaderFlags[i]
        
        tmp <-  x[valueHeaderFlags[i]]
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
    
    
    for (i in seq_along(tablesAll)) {
        v <- tablesAll[i]
        if (i > length(tableHeaderFlags)) {
          next
        }
        index <- tableHeaderFlags[i]
        if(index < max(flagsAll)) {
            index <- index : (flagsAll[which(flagsAll>index)][1]-1)
        } else {
            index <- index : length(x)
        }
        content <- x[index]
    
        ### putting all comments just after table name
        index <- which(substr(content, 1,1) == ">")
        
        header <- content[c(1,index)]
        tableContent <-  content[-c(1,index)]
        ## replacing tabs by spaces
        tableContent <- gsub("([\t])"," ", tableContent)
        
        # Dealing with strings with quotes
        tmp <- gsub('"',"QUOTE", tableContent)
        tmp_df <- matrix(ncol = 3, nrow = length(tmp))
        if(!identical(tableContent, tmp)) { ## only if necessary
            for (j in seq_along(tableContent)) {
                y <-  str_match(tmp[j], 'QUOTE(.*)QUOTE')
                tmp_df[j,1:2] <- y
                tmp_df[j,3] <- repl <- paste0("str", j)
                tmp[j] <- gsub(y[,1], repl, tmp[j])
            }
            tableContent <- tmp
        } else {
            rm(list = c("tmp", "tmp_df"))  
        }
    
    
    
    ### parsing table
        tableContent <- strsplit(tableContent, " ")
        tableContent <- lapply(tableContent, function(x) x[which(nchar(x) > 0)])
        if(exists("tmp")) {
            for (j in seq_along(tmp)) {
            tableContent[[j]] <- gsub(tmp_df[j,3], tmp_df[j,2], tableContent[[j]]) 
            
            }
        }
    
            ## removing empty lines
        tableContent <- tableContent[which(lapply(tableContent,
                                                  function(x) length(x))>0)]
        
        # ### adding row names for some tables in Biomass Succession main inputs
        # if(type == "BiomassSuccession") {
        #   names(tableContent) <- as.character(lapply(tableContent, function(x) x[1]))
        #   tableContent <- lapply(tableContent, function(x) x[-1])
        # }
    
        nElement <- as.numeric(lapply(tableContent, function(x) length(x)))
        if(length(tableContent) > 1) {
            if(nElement[1] < nElement[2]) {
                colN <- as.character(tableContent[[1]])
                tableContent <- tableContent[-1]
                names(tableContent) <- as.character(lapply(tableContent,
                                                           function(x) x[1]))
                if(v %in% c("AvailableLightBiomass", "MinRelativeBiomass")) {
                    tableContent <- lapply(tableContent,
                                           function(x) x[-1])
                }
                
            } else {
                colN <- paste0("V", 1:nElement[1])  
            }
        } else {
            colN <- paste0("V", 1:nElement[1])  
        }
        
        rowN <- names(tableContent)
        
        
        
        ### converting to data.frame
        tableContent <-  do.call("rbind", tableContent)
        
        ### converting to numerical if possible
        tmp <- apply(tableContent, 2, function(x) suppressWarnings(as.numeric(x)))
        
        if(class(tmp)=="matrix") {
            numericIndex <- which(apply(tmp, 2, function(x) sum(is.na(x))==0))  
            for (j in 1:ncol(tmp)) {
                if(is.na(sum(tmp[,j]))) {
                    y <- tableContent[,j]
                
                } else {
                    y <- tmp[,j]
                }
                if(j == 1) {
                    df <- data.frame(y, stringsAsFactors = F)
                } else {
                    df <- data.frame(df, y)
                }
            }
            colnames(df) <- colN 
            if(!is.null(rowN)) {
                rownames(df) <- rowN  
            }
            tableContent <- df
        } else { ## for vectors
            numericIndex <- which(!is.na(tmp)) 
            for (j in seq_along(tmp)) {
                if(is.na(tmp[j])) {
                    y <- tableContent[j]
                
                } else {
                    y <- tmp[j]
                }
                if(j == 1) {
                    df <- data.frame(y, stringsAsFactors = F)
                } else {
                    df <- data.frame(df, y)
                }
            }
            colnames(df) <- colN 
            if(!is.null(rowN)) {
                rownames(df) <- rowN  
            }
            tableContent <- df
        }
        
        
        out[[v]] <- list()
        out[[v]][["header"]] <- header
        out[[v]][["table"]] <- tableContent
        rm(list = c("tmp", "colN"))
    }
    return(out)
}

#####


################################################################################
################################################################################
#### create a list of tables for all Biomass Succession main inputs

SpeciesParameterFetch <- function(bsMain,
                                  #forCS,
                                  aidbURL = aidbURL) {
    
    require(dplyr)
    
    bsInputs <- bsMain$SpeciesParameters$table
    spp <- sppConvert(bsMain$SpeciesParameters$table[,1],
                      inputCode = "LANDIS",
                      aidbURL = aidbURL)
    
    ### Species, Leaf longevity, and mortality/growth shape parameters
    out <- bsInputs[,c(1,2,4)]
    
    ### replace leaf longevity with CBM AIDB values
    ## [placeholder]
    ## [placeholder]
    ## [placeholder]
    
    ### merchantable min age, + Merch Curve shape params a and b
    out[,4:6] <- NA 
    
    
    url <- paste(aidbURL, "tblSpeciesTypeDefault.txt", sep="/")
    if(strsplit(url, "/")[[1]][1] %in% c("https:", "http:")) {
      ### Proportion of Non-merch to FastAG - BranchesToBranchSnag
      ### + sppID BranchesToBranchSnag
      
      bioNonMerchSpecies <- read.csv(text = getURL(url))
      
    } else {
      ### Proportion of Non-merch to FastAG - BranchesToBranchSnag
      ### + sppID BranchesToBranchSnag
      bioNonMerchSpecies <- read.csv(url)
    }
    
    index <- match(spp, bioNonMerchSpecies$SpeciesTypeName)
    ### BranchesToBranchSnag
    out[,7]  <- bioNonMerchSpecies[index, "BranchesToBranchSnag"]
    sppID <- bioNonMerchSpecies[index,"SpeciesTypeID"]
    
    # ### growth shape param (to be tested)
    # out[,8] <- bsInputs[,5]
    
    return(out)
}



################################################################################
################################################################################
### Dompools - "Proportion of the decayed material that goes to the atmosphere'

DomFetch <- function(aidbURL = aidbURL) {
    
    url <- paste(aidbURL, "tblDOMParametersDefault.txt", sep="/")
    if(strsplit(url, "/")[[1]][1] %in% c("https:", "http:")) {
        df <- read.csv(text = getURL(url))
    } else {
        df <- read.csv(url)
    }
    
    index <- 1:10
    PropToAtmosphere <- df[index, "PropToAtmosphere"]
    OrganicMatterDecayRate <- df[index, "OrganicMatterDecayRate"]
    amountAtT0 <- rep(NA, length(index))
    Q10 <- df[index, "Q10"]
    names(PropToAtmosphere) <-
        names(OrganicMatterDecayRate) <-
        names(amountAtT0) <-
        names(Q10) <- index
    return(list(PropToAtmosphere = PropToAtmosphere,
                OrganicMatterDecayRate = OrganicMatterDecayRate,
                amountAtT0 = amountAtT0,
                Q10 = Q10))
}

################################################################################
################################################################################
### EcoSppDOMParameters - "Base decay rates and Q10"
### Initial stocks still has to be set

EcoSppDOMParametersFetch <- function(sppNames, landtypeNames) {
    df <- as.data.frame(do.call("cbind", DomFetch(aidbURL = aidbURL)))
    df[,"poolID"] <- rownames(df)
    df <- expand.grid(spp = sppNames,
                      landtype = landtypeNames,
                      poolID = rownames(df)) %>%
        merge(df) %>%
        select(landtype, spp, poolID,
               OrganicMatterDecayRate, amountAtT0,
               Q10) %>%
        arrange(spp, landtype, poolID)
    return(df)
    
}

################################################################################
################################################################################
### Proportion of physical turnover transferred from a biomass pool to a 
### specific DOM or soil pool, or between certain DOM or soil pools.

ForCSProprotionsFetch <- function(landtypes,
                                  landtypes_AT,
                                  aidbURL = aidbURL) {
    require(dplyr)
    spu <- unique(spuFetch(landtypes, landtypes_AT,
                           aidbURL = aidbURL,
                           spuURL = spuURL))
    
    ### CBM spu and ecozone tables
    if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
        tblEcoBoundaryDefault <- read.csv(text = getURL(paste(aidbURL,
                                                          "tblEcoBoundaryDefault.txt", sep="/")))
        tblSPUDefault <- read.csv(text = getURL(paste(aidbURL,
                                                      "tblSPUDefault.txt", sep="/")))
        tblSpeciesTypeDefault <- read.csv(text = getURL(paste(aidbURL,
                                                              "tblSpeciesTypeDefault.txt", sep="/"))) 
        tblSlowAGtoBGTransferRate <- read.csv(text = getURL(paste(aidbURL,
                                                                  "tblSlowAGtoBGTransferRate.txt", sep="/"))) 
    } else {
        tblEcoBoundaryDefault <- read.csv(paste(aidbURL,
                                                "tblEcoBoundaryDefault.txt", sep="/"))
        tblSPUDefault <- read.csv(paste(aidbURL,
                                        "tblSPUDefault.txt", sep="/"))
        tblSpeciesTypeDefault <- read.csv(paste(aidbURL,
                                                "tblSpeciesTypeDefault.txt", sep="/")) 
        tblSlowAGtoBGTransferRate <- read.csv(paste(aidbURL,
                                                    "tblSlowAGtoBGTransferRate.txt", sep="/")) 
    }
    
    
    spuIndex <- which(tblSPUDefault$SPUID == spu)
    ecoID <- tblSPUDefault[11,"EcoBoundaryID"]
    
    ###
    ecoBoundaryDefault <- tblEcoBoundaryDefault %>%
        filter(EcoBoundaryID == ecoID) %>%
        mutate(BiomassFine = NA, ## didn't find in AIDB, thus NA
               BiomassCoarse = NA, ## didn't find in AIDB, thus NA
               SlowAGtoSlowBG = as.numeric(tblSlowAGtoBGTransferRate)) %>%
        select(BiomassFine,
               BiomassCoarse,
               SlowAGtoSlowBG,
               SoftwoodStemSnagToDOM, # hardwood and softwood values are identical)
               SoftwoodBranchSnagToDOM)
    return(ecoBoundaryDefault)
}

################################################################################
################################################################################
### Fetching disturbance matrices  (some post-processing is necessary)

DMFetch <- function(landtypes,
                    landtypes_AT,
                    aidbURL = aidbURL,
                    #from, # either 'biomass' or 'DOM'
                    forCS_type, # can be NULL, "fire", "other"
                    dmID = NULL)  {  ## if NULL, fetches default disturbance matrix (generally a the 'fire' one)




    if(is.null(dmID)) {
        spu <- unique(spuFetch(landtypes, landtypes_AT,
                               aidbURL = aidbURL,
                               spuURL = spuURL))
        if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
            tblDMAssociationSPUDefault <- read.csv(text = getURL(paste(aidbURL,
                                                                   "tblDMAssociationSPUDefault.txt", sep="/")))
        } else {
            tblDMAssociationSPUDefault <- read.csv(paste(aidbURL,
                                                         "tblDMAssociationSPUDefault.txt", sep="/"))
        }
        df <- tblDMAssociationSPUDefault %>%
            filter(SPUID == spu)

        dmID <- df$DMID
    }

    if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
        tblDMValuesLookup <- read.csv(text = getURL(paste(aidbURL,
                                                          "tblDMValuesLookup.txt", sep="/")))
        tblSourceName <-  read.csv(text = getURL(paste(aidbURL,
                                                       "tblSourceName.txt", sep="/")))
        
        tblSinkName <-  read.csv(text = getURL(paste(aidbURL,
                                                     "tblSinkName.txt", sep="/")))
    } else {
        tblDMValuesLookup <- read.csv(paste(aidbURL,
                                            "tblDMValuesLookup.txt", sep="/"))
        tblSourceName <-  read.csv(paste(aidbURL,
                                         "tblSourceName.txt", sep="/"))
        
        tblSinkName <-  read.csv(paste(aidbURL,
                                       "tblSinkName.txt", sep="/"))
    }
   
    dm <- tblDMValuesLookup %>%
        filter(DMID == dmID)


   
    tblSourceName <- filter(tblSourceName,
                            DMStructureID == 2)
    tblSinkName <- filter(tblSinkName,
                          DMStructureID == 2)

    ## setting up data.frame, default value is 0
    df <- as.data.frame(matrix(0,
                               nrow = max(tblSourceName$Row),
                               ncol = max(tblSinkName$Column)))

    rownames(df)[tblSourceName$Row] <- as.character(tblSourceName$Description)
    colnames(df)[tblSinkName$Column] <- as.character(tblSinkName$Description)

    ## filling in values
    for(i in 1:nrow(dm)) {
        y <- dm[i,]
        df[y$DMRow, y$DMColumn] <- y$Proportion
    }
    dmCBM <- df

    if(!is.null(forCS_type)) {

        domNames <- list("Very Fast Aboveground" = "Aboveground Very Fast DOM",
                         "Very Fast Belowground" = "Belowground Very Fast DOM",
                         "Fast Aboveground" = "Aboveground Fast DOM",
                         "Fast Belowground" ="Belowground Fast DOM",
                         "Medium" = "Medium DOM",
                         "Slow Aboveground" = "Aboveground Slow DOM",
                         "Slow Belowground" = "Belowground Slow DOM",
                         "Stem Snag" = c("Softwood Stem Snag", "Hardwood Stem Snag"),
                         "Other Snag" = c("Softwood Branch Snag", "Hardwood Branch Snag")
        )
        biomassNames <- list("Merchantable wood" = c("Softwood Merchantable",
                                                     "Hardwood Merchantable"),
                             "Foliage" = c("Softwood Foliage",
                                           "Hardwood Foliage"),
                             "Other wood" = c("Softwood Other",
                                              "Softwood Submerchantable",
                                              "Hardwood Other",
                                              "Hardwood Submerchantable"),
                             "Coarse Root" = c("Softwood Coarse Roots",
                                               "Hardwood Coarse roots"),
                             "Fine Root" = c("Softwood Fine Roots",
                                             "Hardwood Fine Roots"))




        dmDOM <- dmCBM[which(rownames(dmCBM) %in% unlist(domNames)),]
        dmBiomass <- dmCBM[which(rownames(dmCBM) %in% unlist(biomassNames)),]

        fromPoolDOM <- fromPoolDOMNames <- rep(NA, nrow(dmDOM))
        for (i in seq_along(domNames)) {
            index <- which(rownames(dmDOM) %in% domNames[[i]])
            fromPoolDOM[index] <- i
            fromPoolDOMNames[index] <- names(domNames)[i]
        }
        fromPoolBiomass <- fromPoolBiomassNames <- rep(NA, nrow(dmBiomass))
        for (i in seq_along(biomassNames)) {
            index <- which(rownames(dmBiomass) %in% biomassNames[[i]])
            fromPoolBiomass[index] <- i
            fromPoolBiomassNames[index] <- names(biomassNames)[i]
        }


        ###############################
        #### DOM
        # From any pool to Gas
        toAirDOM <- apply(dmDOM, 1, function(x) sum(x["CO2"] + x["CH4"] + x["CO"] + x["NO2"]))
        # from snags to the ground (stem to medium or other to fast above)
        toDOMDOM <- toFPSDOM <- toAirDOM
        toDOMDOM[] <- toFPSDOM[] <- NA
        index <- which(fromPoolDOMNames == "Stem Snag")
        toDOMDOM[index] <- dmDOM[index,"Medium Soil C"]
        index <- which(fromPoolDOMNames == "Other Snag")
        toDOMDOM[index] <- dmDOM[index, "Above Ground Fast soil C"]
        # from aboveground pools and snags to FPS
        toFPSDOM <- dmDOM[,"products"]
        toFPSDOM[-which(names(domNames) %in% c("Medium", "Slow Aboveground"))] <- NA

        ###############################
        #### Biomass
        toAirBiomass <- apply(dmBiomass, 1, function(x) sum(x["CO2"] + x["CH4"] + x["CO"] + x["NO2"]))
        toDOMBiomass <- apply(dmBiomass, 1, function(x) sum(x[13:23]))
        toFPSBiomass <- dmBiomass[,"products"]


        # formating ForCS dm
        dmLANDIS_DOM <- data.frame(fromDom = fromPoolDOM,
                                   toAir = toAirDOM,
                                   toDOM = toDOMDOM,
                                   toFPS = toFPSDOM)
        dmLANDIS_biomass <- data.frame(fromBiomass = fromPoolBiomass,
                                       toAir = toAirBiomass,
                                       toFPS = toFPSBiomass,
                                       toDOM = toDOMBiomass)

        # removing lines with no transfer
        index <- which(rowSums(dmLANDIS_DOM[,2:4], na.rm = T) > 0)
        dmLANDIS_DOM <- dmLANDIS_DOM[index,]
        index <- which(rowSums(dmLANDIS_biomass[,2:4], na.rm = T) > 0)
        dmLANDIS_biomass <- dmLANDIS_biomass[index,]

        # averaging values between softwoods and hardwoods
        dmLANDIS_biomass <- dmLANDIS_biomass %>%
            group_by(fromBiomass) %>%
            summarize(toAir = mean(toAir),
                      toFPS = mean(toFPS),
                      toDOM = mean(toDOM))
        dmLANDIS_biomass <- as.data.frame(dmLANDIS_biomass)


        # removing duplicate entries (for hardwood and softwood)
        # (if different, likely to produce error in ForCS)
        dmLANDIS_DOM <- distinct(dmLANDIS_DOM)
        # replacing NA with zeros
        dmLANDIS_DOM[is.na(dmLANDIS_DOM)] <- 0

        # naming rows
        rownames(dmLANDIS_DOM) <- names(domNames)[dmLANDIS_DOM$fromDom]
        rownames(dmLANDIS_biomass) <- names(biomassNames)[dmLANDIS_biomass$fromBiomass]

        ### #  must change indices for Biomass pools
        # >> Indices to be used when referring to biomass pools
        # >> 1. Merchantable wood
        # >> 2. Foliage
        # >> 3. Other wood
        # >> 5. Coarse Root
        # >> 6. Fine Root
        index <- which(dmLANDIS_biomass$fromBiomass > 3)
        dmLANDIS_biomass[index, "fromBiomass"] <- dmLANDIS_biomass[index, "fromBiomass"] + 1

        return(list(dmCBM,
                    fromDOM = dmLANDIS_DOM,
                    fromBiomass = dmLANDIS_biomass))

    }
}



################################################################################
################################################################################
### Fetching allometric parameters for belowground biomass and fine & coarse root turnover

rootBiomassParamsFetch <- function(spp, landtypes_AT, breaks,
                                   aidbURL = aidbURL,
                                   landisURL = "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master") {  ### estimates from Li et al 2003
    
    
    library(readr)
    
  #### fetching species types
  
    vegCodesURL <- paste(landisURL, "vegCodes.csv", sep="/")
    
    #vegCodes <- read.csv(text = getURL(vegCodesURL))  
    vegCodes <- as.data.frame(read_csv(url(vegCodesURL)))
    

    if(strsplit(aidbURL, "/")[[1]][1] %in% c("https:", "http:")) {
        
        tblSpeciesTypeDefault <- read.csv(text = getURL(paste(aidbURL,
                                                              "tblSpeciesTypeDefault.txt", sep="/")))
    } else {
        tblSpeciesTypeDefault <- read.csv(paste(aidbURL,
                                                "tblSpeciesTypeDefault.txt", sep="/"))
    }
    
    
    ### limiting the number of breaks to 6 (including zero)
    if(max(breaks)>10000) {
      index <- which(breaks>10000)
      breaks <- c(breaks[-index], breaks[max(index)])
    }
    
    
    #### midpoints
    mid <- breaks[-length(breaks)]+diff(breaks)/2
    mid <- cbind(breaks[-length(breaks)], mid)
    lt <- landtypes_AT[which(landtypes_AT[,1] %in% c("Yes", "yes", "Y", "y", 1, T)),2]
    
    #### identifying hardwoods and softwoods
    index <- match(spp, tblSpeciesTypeDefault$SpeciesTypeName)
    hw_sw <- ifelse(tblSpeciesTypeDefault[index, "ForestTypeID"] == 1, "sw", "hw")
    hw_sw <- cbind(spp, hw_sw)
    
    #### generating empty data.frame
    df <- expand.grid(species = names(spp),
                      breaks = breaks[-length(breaks)])
    
    # making sure min biomasses are ordered
    df <- df %>%
        arrange(species, breaks)
    
    
    hw_sw <- hw_sw[match(df$species, row.names(hw_sw)), "hw_sw"]
    mid <- mid[match(df$breaks, mid[,1]),"mid"]
    df[,"PropFineRoot"] <- df[,"Root:Shoot"] <-  NA
    for (i in 1:nrow(df)) {
        mid_mgPerHa <- mid[i]/10
        
        if(hw_sw[i] == "sw") {
            rootShootRatio <- 0.222
        }
        if(hw_sw[i] == "hw") {
            rootShootRatio <- (1.576 * mid_mgPerHa^0.615)/mid_mgPerHa
        }
        
        df[i,"Root:Shoot"] <- rootShootRatio
        
        bgb_mgPerHa <- mid_mgPerHa*rootShootRatio
        
        df[i,"PropFineRoot"] <- 0.072 + 0.354 * exp(-0.060*bgb_mgPerHa)
    }
    
    #### matching species in tblSpeciesTypeDefault
    sppNames <- spp[match(df$species, names(spp))]
    index <- match(sppNames, tblSpeciesTypeDefault$SpeciesTypeName)
    
    df[,"FRturnover"] <- tblSpeciesTypeDefault[index, "FineRootTurnPropSlope"]
    df[,"CRturnover"] <- tblSpeciesTypeDefault[index, "CoarseRootTurnProp"]
    
    ### rounding values
    df[,c("Root:Shoot", "PropFineRoot")] <- round(df[,c("Root:Shoot", "PropFineRoot")], 3)
    
    ### removing redundant lines
    iLast <- 1
    for(i in 2:nrow(df)) {
        x <- df[i, c(1,3:6)]
        y <- df[iLast, c(1,3:6)]
        row.names(x) <- row.names(y) <- 1
        if(identical(x, y)) {
            df[i,] <- NA
        } else {
            iLast <- i
        }
    }
    df <- df[complete.cases(df),]
    
    
    ### repeating table for each landtype
    for(i in seq_along(lt)) {
        tmp <- data.frame(landtype = lt[i],
                          df)
        if (i == 1) {
            finalTable <- tmp
        } else {
            finalTable <- rbind(finalTable, tmp)
        }
    }
    row.names(finalTable) <- 1:nrow(finalTable)
    
    return(finalTable)
    
}

################################################################################
################################################################################
### Writing the content of list 'forCS' (headers and tables) to file

forCS_writeToFile <- function(x, file, version) {
    sectionNames <- names(x)
    
    version31up <- as.numeric(version) >= 3.1
    
    if(version31up) {
        tablesDM <- c("DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                      "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass")
        sectionNames <- sectionNames[!sectionNames %in% tablesDM]
        
        x_dm <- x[tablesDM]
        x <- x[sectionNames]
         
    }
    
    ## intro section (main ForCS input file)
    sink(file)
    
    cat('LandisData  "ForC Succession"')
    cat("\n")

    for (i in 1:5) {
        cat("\n")
        cat(paste(sectionNames[i], x[i]))
        cat("\n")
    }
    if(version31up) {
      
      cat("\n")
      cat("DisturbanceMatrixFile	./ForCS_DM.txt")
      cat("\n")
      
    }
    sink()
    
    
    
    for (i in 6:length(sectionNames)) {
        sName <- sectionNames[i]
        sink(file, append = T)
        cat("\n")
        
        cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
        cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
        cat(">>>>>>>>>>\n")
        cat(paste(x[[i]][["header"]], collapse = "\n"))
        cat("\n")
        
        sink()
        write.table(x[[i]][["table"]], file = file,
                    append = T,
                    row.names = ifelse(sName %in% c("AvailableLightBiomass"),
                                       T, F),
                    col.names = ifelse(sName %in% c("AvailableLightBiomass"),
                                      T, F),
                    sep = "\t",
                    quote = ifelse(sName %in% "DOMPools", T, F),
                    #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                    eol = "\n") #default line endings on windows system.)
        
    }
}

forCS_dm_writeToFile <- function(x, file) {
    sectionNames <- names(x)
    
    tablesIndex <- which(sectionNames %in% c("DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                                             "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass"))
   
    
    ## intro section
    sink(file)
    cat('LandisData "ForC Succession"')
    cat("\n")
    sink()
    
    for (i in tablesIndex) {
      sName <- sectionNames[i]
      sink(file, append = T)
      cat("\n")
      
      cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
      cat(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n")
      cat(">>>>>>>>>>\n")
      cat(paste(x[[i]][["header"]], collapse = "\n"))
      cat("\n")
      
      sink()
      write.table(x[[i]][["table"]], file = file,
                  append = T,
                  row.names = F,
                  col.names = F,
                  sep = "\t",
                  quote = F,
                  #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                  eol = "\n") #default line endings on windows system.)
      
    }
  }

### 
###############################################################################
### Producing Climate Data - Mean annual temperature by landtype
########

tMean_fetch <- function(landtypes, landtypes_AT,
                        scenario = "baseline", ## or "RCP45", "RCP85, etc...
                        area,
                        t0,
                        timestep = 10,
                        writeToFile = F,
                        outputTable = F) {
    
    a <- area


    if(is.logical(writeToFile)) {
       if(writeToFile) {
           file <- "ForCSClimateInput.txt"
       }
    } else {
        file <- writeToFile
        writeToFile <- T
    }
    
    print("Fetching and formatting mean annual temperature ...")
    
    # fetching and formatting observation data
    tMean <- getData('worldclim', var="tmean", res=5)
    tMean <- projectRaster(tMean, landtypes, method = 'ngb')
    tMean <- resample(tMean, landtypes)
    # averaging year
    tMean_baseline <- mean(tMean/10)
    
    # computing zonal (landtypes) statistics
    index <- match(values(landtypes), landtypes_AT$V2)
    # 
    tMean_landtype_obs <- zonal(tMean_baseline, landtypes, fun = "mean")
    
    
    if(scenario != "baseline") {## the fetch local .csv obtained from Ouranos
      
      tMean_project <- read.csv(paste0("../climate/Tmean_", a, ".csv"))
      tMean_simAverage <- tMean_project %>%
        filter(tMean_project$ï..Annee <= 2000 &
                 tMean_project$ï..Annee >= 1970) %>%
        summarise(meanObs = mean(Obs),
                  mean45 = mean(rcp45.Avg),
                  mean85 = mean(rcp85.Avg))
      
      if(scenario == "RCP45") {
        tMean_simAverage <- tMean_simAverage$mean45
      }
      if(scenario == "RCP85") {
        tMean_simAverage <- tMean_simAverage$mean85
      }
      
      ### splicing time series
      breaks <- seq(from = t0,
                    to = max(tMean_project$ï..Annee)+timestep,
                    by = timestep)
      
      tMean_sim <- tMean_project[,c("ï..Annee", paste0(tolower(scenario), ".Avg"))]
      tMean_sim <- data.frame(year = tMean_sim[,1],
                              tMean = tMean_sim[,2]) %>%
        filter(year >= t0) %>%
        mutate(bin = cut(year,
                          breaks = breaks,
                          include.lowest = T, right = F)) %>%
        group_by(bin) %>%
        summarise(tMean_delta = mean(tMean) - tMean_simAverage)
      
      ### replacing and updating values in data.frame
      for(i in seq_along(breaks[-length(breaks)])) {
        tmp <- data.frame(timestep = breaks[i] - t0,
                          zone = tMean_landtype_obs[,"zone"],
                          mean = tMean_landtype_obs[,"mean"] + as.numeric(tMean_sim[i, "tMean_delta"]))
        if(i == 1) {
          tMean_landtype <- tmp
        } else {
          tMean_landtype <- rbind(tMean_landtype, tmp)
        }
      }

    } else {
      tMean_landtype <- data.frame(timestep = 0,
                                   tMean_landtype_obs)
    }
    
    #rounding
    tMean_landtype[,"mean"] <- round(tMean_landtype[,"mean"], 3) 
    
    if(writeToFile) {
    
        # formatting and writing to file

        sink(file)
        
        cat('LandisData "ForC Succession"')
        cat("\n")
        cat("\n")
        cat("ClimateTable")
        cat("\n")
        cat(">>Time\tEco\tAvgT\n")
        cat(">>Step\t\t(C)\n")
        sink()
        
        write.table(tMean_landtype, file,
                    append = T,
                    row.names = F,
                    col.names = F,
                    sep = "\t",
                    quote = F,
                    #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                    eol = "\n") #default line endings on windows system.)
    }
    
    if(outputTable) {
        return(tMean_landtype)    
    }
    
}

      




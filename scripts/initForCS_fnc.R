
###################### a wrapper that uses several of the functions above to create a formatted input file
#### for Forest Carbon Succession from Biomass Succession input files and CBM Archive Index Database (AIDB)
initForCS <- function(forCSInput, ### a formatted Forest Carbon Succession input file
                      bsMainInput,  ### Biomass succession main inputs
                      bsDynInput, ### Biomass succession dynamic inputs
                      landtypes,
                      landtypes_AT,
                      climate = F,
                      spinup = F,
                      version = "3.1",
                      scenario,
                      t0,
                      allometry,
                      inputOffset = 0,
                      interpolate = F,
                      alignT0withBaseline = F,
                      valuesSingleAll = c("Timestep", "SeedingAlgorithm", "ForCSClimateFile",
                                          "InitialCommunities", "InitialCommunitiesMap"),
                      tablesAll = c("ForCSOutput", "SoilSpinUp", "AvailableLightBiomass",
                                    "LightEstablishmentTable", "arameters",
                                    "DOMPools", "EcoSppDOMParameters", "ForCSProportions",
                                    "DisturbFireTransferDOM", "DisturbOtherTransferDOM",
                                    "DisturbFireTransferBiomass", "DisturbOtherTransferBiomass",
                                    "ANPPTimeSeries", "MaxBiomassTimeSeries",
                                    "EstablishProbabilities", "RootDynamics",
                                    "SnagData"), ...) {### other arguments may be required for some functions
    
    print("Fetching Landis inputs and templates...")
    ### fetching source formatted Landis Biomass Succession inputs
    bsMain <- landisInputFetch(input = bsMainInput, type = "BSMain")
    bsDyn <- landisInputFetch(bsDynInput, type = "BSDynamics")
    forCS <- landisInputFetch(forCSInput, type = "ForCS")
    # # fetching Forest Carbon succession template file
    # x <- readLines(forCSInput)
    print("Done!")
    
    ### preparing species' list and spatial units
    print("Preparing species' list and spatial units...")
    spp <- sppConvert(bsMain$SpeciesParameters$table[,1],
                      inputCode = "LANDIS", aidbURL = aidbURL)
    spu <- spuFetch(landtypes, landtypes_AT,
                    aidbURL = aidbURL,
                    spuURL = spuURL)
    print("Done!")
    
    ############################################################################
    ### updating AvailableLightBiomass (identical format)
    print("Preparing / updating 'AvailableLightBiomass'...")
    forCS$AvailableLightBiomass$table <- bsMain$MinRelativeBiomass$table
    # storing landtype names for further use
    lt <- colnames(forCS$AvailableLightBiomass$table)
    print("Done!")
    
    
    
    ############################################################################
    ### updating file names (if necessary)
    print("Preparing / updating 'ForCSClimateFile'...")
    forCS$ForCSClimateFile <-  "forCS-climate.txt"
    print("Done!")
    
    ############################################################################
    ### updating output timestep (if necessary)
    print("Preparing / updating 'ForCSClimateFile'...")
    forCS$ForCSOutput$table <- data.frame(bioPools = 1,
                                          domPools = 1,
                                          flux = 1, ## must be 1
                                          summary = 1) ## must be 1
    print("Done!")
    
    

    
    ############################################################################
    ### updating LightEstablishmentTable (identical format)
    print("Preparing / updating 'LightEstablishmentTable'")
    forCS$LightEstablishmentTable$table <- bsMain$SufficientLight$table
    print("Done!")
    
    ############################################################################
    ### updating SpeciesParameters (different parameters)
    print("Preparing / updating 'SpeciesParameters'")
    forCS$SpeciesParameters$table <- SpeciesParameterFetch(bsMain,
                                                           aidbURL = aidbURL)
    
    ## minimum age for merchantable stems
    # (should be revised
    
    # merchMinAge <- c("ABIE.BAL" = 20,
    #                  "ACER.RUB" = 20,
    #                  "ACER.SAH" = 20,
    #                  "BETU.ALL" = 20,
    #                  "BETU.PAP" = 20,
    #                  "FAGU.GRA" = 20,
    #                  "LARI.LAR" = 20,
    #                  "PICE.GLA" = 20,
    #                  "PICE.MAR" = 20,
    #                  "PICE.RUB" = 20,
    #                  "PINU.BAN" = 20,
    #                  "PINU.RES" = 20,
    #                  "PINU.STR" = 20,
    #                  "POPU.TRE" = 20,
    #                  "QUER.RUB" = 20,
    #                  "THUJ.SPP.ALL" = 20,
    #                  "TSUG.CAN" = 20)
    # 
    #forCS$SpeciesParameters$table[,4] <- merchMinAge[forCS$SpeciesParameters$table[,1]]
    
    if (allometry) {
      source("../scripts/dhpToAge_fnc.R")
      dhpToAge <- dhpToAge_fnc(sp = spp, landtypes = landtypes)
      source("../scripts/allometry.R")
      vegCodes <- read.csv("../scripts/data/vegCodes.csv")
      sppLandis <- forCS$SpeciesParameters$table[,1]
      
      
      dhp <- 1:100 # cm
      for(i in seq_along(dhpToAge)) {
        fit <-  dhpToAge[[i]][["model"]]
        age <- ((dhp*10)^2)/fit$coefficients
        #age[age<0] <- 0
        #age <- sar_pred(fit, dbh)
        for (sp in dhpToAge[[i]][["sppLandis"]]) {
          ageCorr <- age + dhpToAge[[i]]$ageDelta[[sp]]
          ageCorr <- data.frame(dhp, ageCorr)
          sppAllo <- vegCodes[match(sp, vegCodes$LandisCode), "allometryCode"]
          x <- biomass_tree_fcn(sp = sppAllo,
                                dbh = dhp)
          x <- x %>%
            group_by(Species_en, dbh) %>%
            mutate(ratio = biomass_kg/sum(biomass_kg),
                   biomassTotal_kg = sum(biomass_kg))
          
          
          ### Woody biomass total
          x <- x %>%
            filter(Component_en != "Foliage") %>%
            group_by(Species_en, dbh) %>%
            mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
            filter(Component_en == "Wood") %>%
            mutate(merchProp = volM_to_volTot(dbh = dbh,
                                              dTop = 7,
                                              stumpHeight = .15,
                                              height = 15)) %>%
            mutate(propStem = woodyBiomassRatio*merchProp)  %>%
            merge(ageCorr, by.x = "dbh", by.y = "dhp") %>%
            filter(ageCorr <= 250)

          ## pred
          index <- which.max(x$propStem)
          aParam <- round(x[index, "propStem"], 3) 
          minAge <- round(x[min(which(x$propStem >0)), "ageCorr"])
          bParam <- ifelse(minAge>25, 0.97,
                      ifelse(minAge>20, 0.95, 0.925))
          
          # pred <-  aParam*(1-bParam^x$ageCorr)   
          # plot(x$ageCorr, x$propStem, type = "l", xlim = c(0,250))
          # lines(x$ageCorr, pred, col = "red")
          # predThresh <- pred
          # predThresh[x$age<minAge] <- 0
          # lines(x$age, predThresh, col = "blue")
        
          index <- which(forCS$SpeciesParameters$table[,1] == sp)
          forCS$SpeciesParameters$table[index,4] <- minAge
          forCS$SpeciesParameters$table[index,5] <- aParam
          forCS$SpeciesParameters$table[index,6] <- bParam
        }
      }

      print("Done!")
    } else {
      # Merch Curve shape params a and b (from Dymond et al 2016)
      #forCS$SpeciesParameters$table[,4] <- minAge
      forCS$SpeciesParameters$table[,5] <- NA#0.7546 #Merch curve shape param 'a'
      forCS$SpeciesParameters$table[,6] <- NA#0.983 #Merch curve shape param 'b'
      print("Done!")
      
    }
    
    ############################################################################
    ### Dompools - "Proportion of the decayed material that goes to the atmosphere'
    print("Preparing / updating 'Dompools'")
    tmp <- forCS$DOMPools$table
    tmp[,3] <-  DomFetch(aidbURL = aidbURL)$PropToAtmosphere
    # tmp[which(tmp$V2 == "Slow Aboveground"), 3] <- 0.83
    forCS$DOMPools$table <- tmp
    
    print("Done!")
    
    ############################################################################
    ### EcoSppDOMParameters
    print("Preparing / updating 'EcoSppDOMParameters'")
    forCS$EcoSppDOMParameters$table <- EcoSppDOMParametersFetch(sppNames = names(spp),
                                                                landtypeNames = lt)
    
    # for soil spin-up
    forCS$EcoSppDOMParameters$table[,5] <- 0
    if(spinup) {
       
        forCS$EcoSppDOMParameters$table[,5] <- 0
    } else {
        tmp <- read.csv(paste0(inputPathLandis, "/DOM-initPools_", a, ".csv"))
        tmp <- tmp %>%
            mutate(poolID = as.factor(poolID),
                   landtype = as.factor(landtype))
        # merging in new data
        tmp  <- forCS$EcoSppDOMParameters$table %>%
            merge(tmp, by = c("landtype", "spp", "poolID"),
                  all = T) %>%
            arrange(spp, landtype, poolID) %>%
            mutate(amountAtT0 = ifelse(is.na(amountAtT0.y),
                                       0, amountAtT0.y)) %>%
            dplyr::select(landtype, spp, poolID,
                   OrganicMatterDecayRate,
                   amountAtT0, Q10)
   
        forCS$EcoSppDOMParameters$table <- tmp
    }
    print("Done!")
    
    ############################################################################
    ### ForCSProportions
    print("Preparing / updating 'ForCSProportions'")
    forCS$ForCSProportions$table <- ForCSProprotionsFetch(landtypes, landtypes_AT,
                                                          aidbURL = aidbURL)
    forCS$ForCSProportions$table[,1:2] <- 0.5  ### I couldn't find those values in AIDB
    ### so they are hard coded at this moment.
    print("Done!")
    
    
    
    
    ############################################################################
    ############################################################################
    ############################################################################
    #### disturbance matrices
    print("Preparing / updating Disturbance matrices - 'DisturbFireTransferDOM'")
    
    ### DisturbFireTransferDOM
    dm <- DMFetch(landtypes,landtypes_AT,
                  aidbURL = aidbURL,
                  #from = "DOM",
                  forCS_type = "fire")$fromDOM
    for (i in 1:5) {
        df <-  data.frame(Intensity = i, dm)
        if(i == 1) {
            dm_Fire <- df
        } else {
            dm_Fire <- rbind(dm_Fire, df)
        }
    }
    
    forCS$DisturbFireTransferDOM$table <- dm_Fire
    print("Done!")
    
    ### DisturbOtherTransferDOM
    print("Preparing / updating Disturbance matrices - 'DisturbOtherTransferDOM'")
    dmID <- c(harvest = 135, #"97% clearcut","Sylva CPRS clearcut 97%"
              wind = 227, #"Uprooting and decay for All Eco Boundaries","Uprooting and decay for All Eco Boundaries"
              bda = 427)#, # "DMID 427: Spruce Budworm in QC, Severe annual defoliation, 6yr cumulative defoliation > 85%"
              #bdaSalv = 25) # "Stand Replacing Matrix #25","Insects followed by Salvage Logging Matrix #1 (Stand Replacing). Traditionally used for all ecozones across Canada."
    
    
    for (i in seq_along(dmID)) {
        dt <-names(dmID)[i]
        df <- DMFetch(landtypes,landtypes_AT,
                      aidbURL = aidbURL,
                      #from = "DOM",
                      forCS_type = "other",
                      dmID = dmID[i])$fromDOM
        if(nrow(df) > 0) {
            df <- data.frame(DisturbanceType = dt,
                             df)
                        
            if(i == 1) {
                otherTransferDOM <- df
            } else {
                otherTransferDOM <- rbind(otherTransferDOM, df) 
            }
        }
        rm(df)
    }
    forCS$DisturbOtherTransferDOM$table <- otherTransferDOM
    print("Done!")
    
    ### DisturbFireTransferBiomass
    print("Preparing / updating Disturbance matrices - 'DisturbFireTransferBiomass'")
    dm <- DMFetch(landtypes,landtypes_AT,
                  forCS_type = "fire",
                  aidbURL = aidbURL,)$fromBiomass
    # each lines must sum up to 1, else, C will disappear
    dm[,2:4] <- t(apply(dm[,2:4], 1, function(x) round(x/sum(x), 4)))
    
    for (i in 1:5) {
        df <-  data.frame(Intensity = i, dm)
        if(i == 1) {
            dm_Fire <- df
        } else {
            dm_Fire <- rbind(dm_Fire, df)
        }
    }
    
    forCS$DisturbFireTransferBiomass$table <- dm_Fire
    print("Done!")
    
    ### DisturbOtherTransferBiomass
    print("Preparing / updating Disturbance matrices - 'DisturbOtherTransferBiomass'")
   
    for (i in seq_along(dmID)) {
        dt <-names(dmID)[i]
        df <- data.frame(DisturbanceType = dt,
                         DMFetch(landtypes,landtypes_AT,
                                 #from = "DOM",
                                 forCS_type = "other",
                                 aidbURL = aidbURL,
                                 dmID = dmID[i])$fromBiomass)
        if(i == 1) {
            otherTransferBiomass <- df
        } else {
            otherTransferBiomass <- rbind(otherTransferBiomass, df)
        }
    }
    dm <-otherTransferBiomass
    # each lines must sum up to 1, else, C will disappear
    dm[,3:5] <- t(apply(dm[,3:5], 1, function(x) round(x/sum(x), 4)))
    
    forCS$DisturbOtherTransferBiomass$table <- dm
    print("Done!")
    
    ############################################################################
    #### Dynamic inputs
    
    
    if(interpolate) {
      if(length(unique(bsDyn$year))<2) {
        warning("only one period provided, no interpolation possible")
      } else {
        print("Interpolating dynamic inputs for an annual timestep")
        for (sp in unique(bsDyn$species)) {
          for (l in unique(bsDyn$landtype)) {
            tmp <- filter(bsDyn, landtype == l, species == sp) %>%
              arrange(year)
            
            xout <- min(tmp$year):max(tmp$year)
            
            periodLength <- diff(tmp$year)
            periodLength <- c(periodLength, last(periodLength))
            tmp$year <- tmp$year + periodLength/2
            
            
            
            probEst <- round(as.data.frame(approx(tmp$year, tmp$probEst,  xout = xout))$y, 3)
            probEst[is.na(probEst)] <- probEst[!is.na(probEst)][1]
            maxANPP <- round(as.data.frame(approx(tmp$year, tmp$maxANPP,  xout = xout))$y, 0)
            maxANPP[is.na(maxANPP)] <- maxANPP[!is.na(maxANPP)][1]
            maxB <- round(as.data.frame(approx(tmp$year, tmp$maxB,  xout = xout))$y)
            maxB[is.na(maxB)] <- maxB[!is.na(maxB)][1]
            
            tmp <- data.frame(year = xout, landtype = l, species = sp,
                              probEst, maxANPP, maxB)
            
            
            if(sp == unique(bsDyn$species)[1] &
               l == unique(bsDyn$landtype)[1]) {
              x <- tmp 
            } else {
              x <- rbind(x, tmp)
            }
          }
        }
        x <- arrange(x, year, landtype, species)
        bsDyn <- x
      }
      
    }
    
    
    
    #####
    print("Resetting update year based on an annual timestep...")
    tsCorr <- bsMain$Timestep-1
    tsCorr <- tsCorr - inputOffset
    
    
    f <- function(x) {
      return(as.numeric(x[1])-tsCorr)
    }
    
    
     #### ANPPTimeSeries
    print("Preparing / updating 'ANPPTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "maxANPP")]
    tmp[,"year"] <- apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$ANPPTimeSeries$table <- tmp[tmp$year>=0,]
    # add standard deviation
    forCS$ANPPTimeSeries$table[,"ANPP-Std"] <- 1
    print("Done!")
    
    #### MaxBiomassTimeSeries
    print("Preparing / updating 'MaxBiomassTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "maxB")]
    tmp[,"year"] <-  apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$MaxBiomassTimeSeries$table <- tmp[tmp$year>=0,]
    print("Done!")
    
    #### EstablishProbabilities
    print("Preparing / updating 'MaxBiomassTimeSeries'")
    tmp <- bsDyn[, c("year", "landtype", "species", "probEst")]
    tmp[,"year"] <-  apply(tmp, 1, f)
    tmp <- tmp[tmp$year>=-tsCorr,]
    if(length(unique(tmp$year)) == 1) {
      if(unique(tmp$year) == -tsCorr) {
        tmp$year <- 0
      }
    }
    forCS$EstablishProbabilities$table <- tmp[tmp$year>=0,]
    print("Done!")
  
    ### storing T0 baseline dynamic inputs is alignment with
    ### other scenarios is required
    if(s == "baseline" &
       alignT0withBaseline) {
      dynInputT0 <- list(ANPP = filter(forCS$ANPPTimeSeries$table,
                                       year == 0),
                         maxB = filter(forCS$MaxBiomassTimeSeries$table,
                                       year == 0),
                         SEP = filter(forCS$EstablishProbabilities$table,
                                       year == 0))
    }
    
    if(s != "baseline" &
       alignT0withBaseline) {
      
      dynInputT0 <- out$dynInputT0
      index <- which(forCS$ANPPTimeSeries$table$year == 0)
      forCS$ANPPTimeSeries$table[index,] <- dynInputT0$ANPP
      forCS$MaxBiomassTimeSeries$table[index,] <- dynInputT0$maxB
      forCS$EstablishProbabilities$table[index,] <- dynInputT0$SEP
      
    }

    
    ############################################################################
    #### RootDynamics
    
    ### some of the parameters are in tblSpeciesTypeDefault, but here I'm using
    ### equations from Li et al. 2003
    print("Preparing / updating 'RootDynamics'")
    step <- 2500 ### doesn't work with 2000, appears it has to be >= 2500
    maxBRounded <- ceiling(max(bsDyn$maxB)/step)*step
    breaks <- seq(0, maxBRounded, step) ### max values will be eliminated through the process
    
    
    forCS$RootDynamics$table <- rootBiomassParamsFetch(spp, landtypes_AT,
                                                       aidbURL = aidbURL,
                                                       breaks)
    print("Done!")
    
    ############################################################################
    #### SnagData
    ## removing SnagData section
    print("Preparing / updating 'SnagData'")
    forCS <- forCS[-which(names(forCS)=="SnagData")]
    print("Done!")
    
    ############################################################################
    #### SoilSpinUp
    forCS <- soilSpinUp(forCS,
                        soilSpinUp = spinup,
                        tolerance = 0.5, 
                        maxIter = 100)

    ############################################################################
    #### Writing ForCS parameters to file
    if(spinup) {
        file <- paste0("forCS-input_", a, "_spinup.txt")
    } else {
        file <- paste0("forCS-input_", a, "_", s, ".txt")
    }
    
    print(paste0("Writing updated ForCS inputs to file '", file, "'"))
    forCS_writeToFile(x = forCS, file, version)
    print("Done!")
    
    
    ############################################################################
    #### Writing ForCS_DM parameters to file
    if(as.numeric(version) >= 3.1) {
      file <- paste0("ForCS_DM_", a, "_", s, ".txt")
      print(paste0("Producing forCS disturbance matrices inputs and writing to file '", file, "'"))
      forCS_dm_writeToFile(x = forCS, file)
      
      print("Done!") 
    }
    
    ############################################################################
    #### producing forCS climate input file
    if(climate) {
        file <- paste0("forCS-climate_", a, "_", s, ".txt")#forCS$ForCSClimateFile
        print(paste0("Producing forCS climate inputs and writing to file '", file, "'"))
        tMean_fetch(landtypes, landtypes_AT,
                    area = a,
                    t0 = t0,
                    scenario = s,
                    writeToFile = file,
                    outputTable = F)
        print("Done!") 
    }
    
    
    
    if(alignT0withBaseline) {
      out <- list(dynInputT0 = dynInputT0)
    } else {
      out <- list()
    }
    return(out)
    
}

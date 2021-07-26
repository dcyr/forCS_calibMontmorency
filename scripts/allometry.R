# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # for my Windows machine
# wwd <- paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", Sys.Date() ,sep ="/")
# dir.create(wwd)
# setwd(wwd)
# #################

################################################################################
##### Tree-level from DBH only
# Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# Reference: Ung, C.-H.; Bernier, P.; Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# 
# Parameters for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm)
# Biomass_kg = a * (D_cm^b)
biomass_tree_fcn <- function(sp,
                             dbh,
                             component = "all",
                             paramPath = "../scripts/data") {
    
    require(tidyverse)
    
    params <- read.csv(paste(paramPath,
                                   "Tree-level allometric equations dbh.csv", sep = "/"),
                             fileEncoding = "Windows-1252", skip = 6, header = T)
    # paramTreeDbhHeight <- read.csv(paste(paramPath,
    #                                      "Tree-level allometric equations dbh-height.csv", sep = "/"),
    #                                fileEncoding = "Windows-1252", skip = 5, header = T)
    # paramPlot  <- read.csv(paste(paramPath,
    #                              "Plot-level allometric equations.csv", sep = "/"),
    #                        fileEncoding = "Windows-1252", skip = 9, header = T)
    
    
    x <- params %>%
        filter(Species_en %in% sp) %>%
        merge(data.frame(dbh = dbh)) %>%
        mutate(biomass_kg = a * dbh^b)
        
    return(x)
}

################################################################################
##### Ratio of merchantable vol to total volume
##### Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada.
### Vm/V = R1 + R2*X3 + R3*X3*
### X3 = (T*T/(D*D*((1-0.04365*B2)**2)))*(1+S/H)
# R1 <- 0.9604
# R2 <- -0.1660
# R3 <- -0.7868
### T = Top diameter (inside, cm)
### D = dbh (outside, cm)
### B2 = 0.154 ###
### S = Stump height (m)
### H = total height (m)


volM_to_volTot <- function(dbh, dTop, stumpHeight, height) {
    D <- dbh
    t <- dTop
    S <- stumpHeight
    H <- height
    
    B2 = 0.154 ###
    
    X3 <- (t*t/(D*D*((1-0.4365*B2)^2))) * (1 + S/H)
    
    R1 <- 0.9604
    R2 <- -0.1660
    R3 <- -0.7868
    
    
    ratio = R1 + R2*X3 + R3*X3^2
    ratio[D<t+2] <- 0
    return(ratio)
}


#
# #### test et visualisation
# #### Sourcing scripts
# require(raster)
# require(RCurl)
# source("../scripts/dhpToAge_fnc.R")
# source("../scripts/CBMtoLANDIS_fnc.R", encoding = "Windows-1252")
# a <- "Maskinonge"
# s <- "baseline"
# 
# forCSInput <- paste0("../inputsLandis/forCS-input_", a, "_", s, ".txt")
# landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
# forCS <- landisInputFetch(forCSInput, type = "ForCS")
# vegCodes <- read.csv("../scripts/data/vegCodes.csv")
# spp <- forCS$SpeciesParameters$table[,1]
# sppAllo <- vegCodes[match(spp, vegCodes$LandisCode), "allometryCode"]
# aParams <- forCS$SpeciesParameters$table[,5]
# bParams <- forCS$SpeciesParameters$table[,6]
# 
# dhpToAge <- dhpToAge_fnc(sp = sppAllo, 
#                          landtypes = landtypes)
# 
# sp <- c("Deciduous", "Conifers", "All", "Balsam fir", "American beech", "Black spruce", "Sugar maple", "Trembling aspen")
# dbh <- 1:50
# 
# x <- biomass_tree_fcn(sp = sppAllo, dbh = dbh)
# 
# x <- x %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg),
#            biomassTotal_kg = sum(biomass_kg))
# 
# 
# #### Total aboveground biomass
# require(ggplot2)
# ggplot(data = x, aes(x = dbh, y = biomassTotal_kg, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Tree-level aboveground biomass as a function of DBH",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "total aboveground biomass\n(kg)")
# 
# 
# #### Foliage proportion
# ##################
# df <- x %>%
#     filter(Component_en == "Foliage")
# 
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Foliage proportion of total aboveground biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1,
#               vjust = -0.5, hjust = 1,
#               label = "ForCS hard-coded setting",
#               colour = "grey25")
# 
# 
# ### Woody biomass total
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#     filter(Component_en == "Wood") %>%
#     mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                       dTop = 8,
#                                       stumpHeight = .3,
#                                       height = 20))
# 
# ggplot(data = df, aes(x = dbh, y = woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion")
# 
# #
# 
# 
# png(filename = paste0("propStem_dhp.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=10)
# ggplot(data = df, aes(x = dbh, y = merchProp*woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +
#     labs(title = "Merchantable proportion of woody biomass",
#          subtitle = paste0("Sources:\nM.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018\n",
#                           "Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada."),
#          x = "dbh (cm)",
#          y = "proportion") +
#     theme(plot.subtitle=element_text(size=rel(0.5)))
# dev.off()
# 
# 
# #### Sourcing scripts
# require(raster)
# require(RCurl)
# source("../scripts/dhpToAge_fnc.R")
# source("../scripts/CBMtoLANDIS_fnc.R", encoding = "Windows-1252")
# a <- "Maskinonge"
# s <- "baseline"
# 
# forCSInput <- paste0("../inputsLandis/forCS-input_", a, "_", s, ".txt")
# landtypes <- raster(paste0("../inputsLandis/landtypes_", a, ".tif"))
# forCS <- landisInputFetch(forCSInput, type = "ForCS")
# vegCodes <- read.csv("../scripts/data/vegCodes.csv")
# spp <- forCS$SpeciesParameters$table[,1]
# aParams <- forCS$SpeciesParameters$table[,5]
# bParams <- forCS$SpeciesParameters$table[,6]
# minAge <- forCS$SpeciesParameters$table[,4]
# 
# dhp <- df$dbh
# dhpToAge <- dhpToAge_fnc(sp = spp, 
#                          landtypes = landtypes)
# 
# for (i in seq_along(dhpToAge)) {
#     fit <-  dhpToAge[[i]][["model"]]
#     spp <- dhpToAge[[i]]$sppLandis
#     sppAllo <- vegCodes[match(spp, vegCodes$LandisCode), "allometryCode"]
#     x <- df %>%
#         filter(Species_en %in% sppAllo)
#     dhpToAge <- dhpToAge_fnc(sp = sppAllo, 
#                              landtypes = landtypes)
#     x[,"age"] <- ((x$dbh*10)^2)/fit$coefficients
#     for (j in seq_along(spp)) {
#         sp <- spp[j]
#         #
#         index <- which(forCS$SpeciesParameters$table[,1]==sp)
#         aPar <- aParams[index]
#         bPar <- bParams[index]
#         minA <- minAge[index]
#         
#         spAllo <- sppAllo[j] 
#         y <- x %>%
#             filter(Species_en == spAllo)
#         ageCorr <- y$age + dhpToAge[[i]]$ageDelta[[sp]]
#         ageCorr <- data.frame(dhp = y$dbh, ageCorr)
#         
#         tmp <- biomass_tree_fcn(sp = spAllo,
#                                          dbh = y$dbh)
#         tmp <- tmp %>%
#             group_by(Species_en, dbh) %>%
#             mutate(ratio = biomass_kg/sum(biomass_kg),
#                    biomassTotal_kg = sum(biomass_kg))
#         
#         
#         ### Woody biomass total
#         tmp <- tmp %>%
#             filter(Component_en != "Foliage") %>%
#             group_by(Species_en, dbh) %>%
#             mutate(woodyBiomassRatio = biomass_kg/sum(biomass_kg)) %>%
#             filter(Component_en == "Wood") %>%
#             mutate(merchProp = volM_to_volTot(dbh = dbh,
#                                               dTop = 7,
#                                               stumpHeight = .15,
#                                               height = 15),
#                    minAge = minA) %>%
#             mutate(propStem = woodyBiomassRatio*merchProp)  %>%
#             merge(ageCorr, by.x = "dbh", by.y = "dhp") %>%
#             filter(ageCorr <= 250) %>%
#             mutate(aParam = aPar,
#                    bParam = bPar,
#                    spLandis = sp)
#         
#         if(i == 1 &
#            j == 1) {
#             biomass_tree <- tmp
#         } else {
#             biomass_tree <- rbind(biomass_tree, tmp)
#         }
#     }
# }
#  
# 
# df <- biomass_tree %>%
#     mutate(propStemLandis = aParam*(1-bParam^ageCorr)) %>%
#     mutate(propStemLandis = ifelse(ageCorr < minAge, 0, propStemLandis))
# png(filename = paste0("propStem_age_landis.png"),
#     width = 10, height = 8, units = "in", res = 600, pointsize=10)
#     
#     ggplot(df, aes(x = ageCorr)) +
#         facet_wrap(~spLandis) +
#         geom_line(aes(y = propStemLandis)) +
#         geom_line(aes(y = propStem), linetype = "dotted") +
#         labs(x = "âge",
#              y = "Proportion marchande",
#              title = "Proportion marchande de la biomasse aérienne ligneuse en fonction de l'âge",
#              subtitle = "Proportion simulée par ForCS (en noir), proportion estimées à partir de relations allométrique et de défilement (pointillée) et paramétrage Landis précédent (en bleu).",
#              caption = "Notez les proportions décroissantes passé un certain âge chez certaines espèces (ex. Quercus). Cela s'explique par la forte biomesse présente dans les branches, \nqui ne sont pas considérées par les modèles de défilement (d'où le choix de simuler un plateau).") +
#         geom_line(aes(y = 0.7546*(1-0.983^ageCorr)), colour = "blue") +
#         theme(plot.subtitle=element_text(size= rel(0.7)),
#               plot.caption = element_text(size= rel(0.6)))
#         
# dev.off()

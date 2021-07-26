# rm(list = ls())
# ################################################################################
# home <- path.expand("~")
# home <- gsub("/Documents", "", home) # for my Windows machine
# wwd <- paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", Sys.Date() ,sep ="/")
# dir.create(wwd)
# setwd(wwd)
# source("../scripts/allometry.R")
#################


dhpToAge_fnc <- function(sp,
                         landtypes,
                         dataPath = "../scripts/data") {
    
    
    require(tidyverse)
    #require(sars) 

    df <- read.csv(paste(dataPath, "DHP_AGE_brut_qc.csv", sep = "/")) %>%
        filter(!is.na(AGE),
               AGE <= 350,
               DHP <= 800)

    vegCodes <- read.csv(paste(dataPath, "vegCodes.csv", sep = "/"))
    

    df <- df %>%
        merge(vegCodes, by.x = "ESSENCE", by.y = "qcCode", all.x = T)



    ageDeltaAt1m <- c(ABIE.BAL = 12,
                      ACER.RUB = 4,
                      ACER.SAH = 6,
                      BETU.ALL = 4,
                      BETU.PAP = 5,
                      FAGU.GRA = 16,
                      LARI.LAR = 8,
                      PICE.GLA = 18,
                      PICE.MAR = 12,
                      PICE.RUB = 18,
                      PINU.BAN = 7,
                      PINU.RES = 8,
                      PINU.STR = 10,
                      POPU.TRE = 4,
                      QUER.RUB = 9,
                      THUJ.SPP.ALL = 26,
                      TSUG.CAN = 25)

    
    sppGroup <- c(ABIE.BAL = "borFast",
                  ACER.RUB = "tempFast",
                  ACER.SAH = "tempSlow",
                  BETU.ALL = "tempFast",
                  BETU.PAP = "borFast",
                  FAGU.GRA = "tempSlow",
                  LARI.LAR = "borFast",
                  PICE.GLA = "borSlow",
                  PICE.MAR = "borSlow",
                  PICE.RUB = "borSlow",
                  PINU.BAN = "borFast",
                  PINU.RES = "tempFast",
                  PINU.STR = "tempFast",
                  POPU.TRE = "borFast",
                  QUER.RUB = "tempSlow",
                  THUJ.SPP.ALL = "tempSlow",
                  TSUG.CAN = "tempSlow")
    
    
    df[,"ageDelta"] <- ageDeltaAt1m[df$LandisCode]
    df[,"ageCorr"] <-  df$AGE + df[,"ageDelta"]
    df[,"group"] <-sppGroup[df$LandisCode]
    
    # subsetting within geographical area (buffer 100 km)
    crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    r <- landtypes
    
    ### creating coarse raster (faster processing)
    rCoarse <- r
    rCoarse[] <- NA
    res(rCoarse) <- 5000
    
    rCoarse <- resample(r, rCoarse)
    rCoarse[!is.na(rCoarse)] <- 1
    
    ### extending raster
    rExt <- raster(extend(extent(r), 100000),
                   resolution = res(rCoarse),
                   crs = crs(r))
    
    rCoarse <- resample(rCoarse, rExt)
    rBuffer <- buffer(rCoarse, width = 100000,
                      doEdge = T,
                      dissolved = T)
    rBuffer <- projectRaster(rBuffer, crs = CRS(crs), method = "ngb")



    p <- as(rBuffer, 'SpatialPolygons')  



    cNames <- c("LONGITUDE", "LATITUDE")


    
    xy <- SpatialPointsDataFrame(coords = df[,cNames],
                                 data = df[,which(!colnames(df) %in% cNames)],
                                 proj4string = CRS(crs))
    xy <- over(p, xy)[,c("AGE", "ESSENCE", "DHP", "group")]
    
    ### Chapman-Richards model fit
    CR_Fit <- list()
    for (g in unique(sppGroup)) {
        sppLandis <- names(sppGroup[which(sppGroup == g)])
        #sppLandis <- vegCodes[match(sppLandis, vegCodes$qcCode), "LandisCode"]
        df <- xy[complete.cases(xy),] %>%
            filter(group == g)
        
        dhp <- df$DHP
        dhpSq <- df$DHP^2
        age <-df$AGE
        
        
        print(paste("fitting linear model for", g, "species group"))
        fit <- lm(dhpSq ~ 0 + age)
        #fit <- lm(dhp ~ 0+age)
        # #print(paste("fitting Chapman-Richard model for", g, "species group"))
        
        # plot(x = age, y = df$DHP)
        # lines(x = 1:200, y = predict(fit, data.frame(age = (1:200)))^.5, type = "l", col = "red")
        # lines(x = 1:200, y = predict(fit, data.frame(age = (1:200))), type = "l", col = "blue")
        # 
        # 
        # fit <- sar_chapman(data.frame(a = df$ageCorr, s = df$DHP),
        #             grid_n = 1000,
        #             grid_start = T)
        CR_Fit[[g]] <- list()
        CR_Fit[[g]][["model"]] <- fit
        CR_Fit[[g]][["sppLandis"]] <- sppLandis
        CR_Fit[[g]][["ageDelta"]] <- ageDeltaAt1m[sppLandis]
    }
    return(CR_Fit)
}




# #### test et visualisation
# sp <- c("Deciduous", "Conifers", "All", "Balsam fir", "American beech", "Black spruce", "Sugar maple", "Trembling aspen")
# dbh <- 1:50
# params <- paramTreeDBH
# x <- biomass_tree_fcn(sp, dbh,
#                       paramTreeDBH = params)
# 
# x <- x %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg),
#            biomassTotal_kg = sum(biomass_kg))
# 
# 
# 
# 
# #### Total aboveground biomass
# require(ggplot2)
# ggplot(data = x, aes(x = dbh, y = biomassTotal_kg, group = Species_en, colour = Specie# Biomass_kg = a * D_cm^b * H_m^c
# s_en)) +
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
# 
# 
# 
# png(filename = paste0("propStem_dhp.png"),
#     width = 8, height = 5, units = "in", res = 600, pointsize=10)
# ggplot(data = df, aes(x = dbh, y = merchProp*woodyBiomassRatio, group = Species_en, colour = Species_en)) +
#     geom_line() +# Biomass_kg = a * D_cm^b * H_m^c

#     labs(title = "Merchantable proportion of woody biomass",
#          subtitle = paste0("Sources:\nM.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018\n",
#                           "Honer, T. G., Ker, M. F., & Alemdag, I. S. (1983). Metric timber tables for the commercial tree species of Central and Eastern Canada."),
#          x = "dbh (cm)",
#          y = "proportion") +
#     theme(plot.subtitle=element_text(size=rel(0.5)))
# dev.off()
# 
# 
# ### 
# df <- x %>%
#     filter(Component_en != "Foliage") %>%
#     group_by(Species_en, dbh) %>%
#     mutate(ratio = biomass_kg/sum(biomass_kg)) %>%
#     as.data.frame() %>%
#     select(Species_en, Component_en, dbh, biomass_kg, ratio)
#     
# ggplot(data = df, aes(x = dbh, y = ratio, group = Species_en, colour = Species_en)) +
#     facet_wrap(~Component_en) +
#     geom_line() +
#     labs(title = "Stem proportion of aboveground woody biomass",
#          subtitle = "Source: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018.",
#          x = "dbh (cm)",
#          y = "proportion") +
#     geom_hline(yintercept = 0.1, linetype = "dotted", colour = "grey25") +
#     geom_text(x = max(dbh),
#               y = 0.1, 
#               vjust = -0.5, hjust = 1,
#               label = "ForCS setting",
#               colour = "grey25")

################################################################################
##### Tree-level from DBH only
# Reference: M.-C. Lambert, C.-H. Ung, and F. Raulier 2005. Canadian national biomass equations. Can. J. For. Res 35: 1996-2018. 
# Reference: Ung, C.-H., Bernier, P., Guo, X.-J. 2008. Canadian national biomass equations: new parameter estimates that include British Columbia data. Can. J. For. Res 38:1123-2232.
# 
# Parameters for biomass equation for estimating biomass (Kg, Biomass_kg) at the tree scale from DBH (cm, D_cm) and height (m, H_m)
# Biomass_kg = a * D_cm^b * H_m^c

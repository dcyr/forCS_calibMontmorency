###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("\\\\", "/", home)
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
require(dplyr)

initYear <- 2020
unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
simName <- "ForMont_2022-08-09"
a <- ifelse(grepl("ForMont", simName), "ForMont",
            ifelse(grepl("Hereford", simName), "Forêt Hereford", simName))

areaName <- ifelse(a == "ForMont", "Forêt Montmorency",
                   ifelse(a == "Hereford", "Forêt Hereford", a))

require(ggplot2)
require(dplyr)
require(tidyr)


##########################################################
##### pools
variableLvl <- c("TotalEcosys", "TotalDOM", "ABio", "BBio") ## ordering levels for plotting


mgmtLevels <- list(ForMont = c("CPRS" = "CPRS",
                               "CPI-CP" = "Couvert permanent",
                               "noHarvest" = "Conservation"))

mgmtLvls <- c("CPRS", "Couvert permanent", "Conservation")




cols <- list(ForMont = c("CPRS" = "black",
                         "Couvert permanent" = "dodgerblue3",
                         "Conservation" = "goldenrod3"))

scenRef <- list(ForMont = "CPRS")
################################################################################
################################################################################
################################################################################
outputSummary <- get(load(paste0("../outputCompiled/output_summary_", simName, ".RData")))
fps <- read.csv(paste0("../outputCompiled/output_BioToFPS_", simName, ".csv"))#read.csv("output_BioToFPS_Hereford.csv")#
AGB <- get(load(paste0("../outputCompiled/output_bio_", simName, ".RData")))

################################################################################
outputSummary <- outputSummary %>%
    filter(variable != "mgmtScenarioName") %>%
    mutate(value = as.numeric(value))
outputSummary <- droplevels(outputSummary)




require(ggplot2)
require(dplyr)
require(tidyr)


### pools
df <- outputSummary %>%
    mutate(mgmtScenarioName = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%

    filter(Time >=1,variable %in% variableLvl) %>%

    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
             ##################
             ##plantedSp,
             
            # mgmtID,
             Time, variable) %>%
    summarise(value = mean(value),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
             ##################
             #plantedSp,
             # mgmt,
             Time, variable) %>%
    summarise(valueTotal = sum(value*mgmtArea_ha),
              mgmtArea_ha = sum(mgmtArea_ha)) %>%
    mutate(value = valueTotal/mgmtArea_ha) %>%
    as.data.frame()


df <- df %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
             ##################
             #plantedSp, mgmt,
             Time) %>%
    summarise(valueTotal = sum(valueTotal),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    mutate(value = valueTotal / mgmtArea_ha,
           variable = "TotalEcosys") %>%
    as.data.frame()%>%
    rbind(df) %>%
    mutate(variable = factor(variable, levels = variableLvl)) 



x <- df %>%
    filter(areaName == a,
           mgmtScenario == scenRef[[a]])
cNames <- colnames(df)
cNames <- cNames[-which(cNames %in% c("mgmtScenario", "mgmtScenarioName", "valueTotal", "value", "mgmtArea_ha"))]

dfRef <- merge(df, x,
             by = cNames,
             suffixes = c("",".ref"))

cNames <- c(colnames(df), "value.ref")
dfRef <- dfRef[,cNames]

ref <- df


if(a == "Hereford") {
    p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,
                   colour = mgmtScenario)) 
}

if(a == "ForMont") {
    
   p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact,
                   colour = mgmtScenarioName))
}

p <- p +
    facet_grid(variable ~ scenario, scale = "free") +
    theme_bw() +
    scale_color_manual(name = "Scénario\nd'aménagement",
                       values = cols[[a]]) +

    geom_line(size = 0.5) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la densité moyenne en carbone",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("ABio : Biomasse aérienne",
                          "\nBBio : Biomasse souterraine",
                          "\nTotalDOM : Bois mort, litière, humus et sol minéral"))


    
png(filename= paste0("pools_", simName, ".png"),
    width = 8, height = 6, units = "in", res = 600, pointsize=10)
    
    print(p)

dev.off()



### fluxes


variableLvl <- c("DelBio",  "Turnover",
                 "NetGrowth",  "NPP",
                 "Rh",  "NEP",
                 "NBP")

caption <- c("DelBio: Annual change in biomass stocks",
             "Turnover: Annual transfer of biomass (above-and belowground) to dead organic matter and soil pools before disturbances occur",
             "NetGrowth: Change in biomass from growth alone: the difference between the biomass at the beginning and the end of the growth routine in the timestep. This value could be negative\n         as the stand ages and mortality outpaces growth. DelBio and NetGrowth will be the same when there are no losses caused by disturbances.",
             "NPP : Net Primary Production (includes above and belowground). This includes growth and replacement of litterfall and annual turnover, i.e., the sum of NetGrow and turnover.",
             "Rh : Heterotrophic respiration. This is the sum of the 'To Air' fluxes through decomposition, not disturbance.",
             "NEP: Net Ecosystem Productivity. NPP minus Rh.",
             "NBP : Net Biome Productivity. NEP minus losses from the ecosystem due to disturbances. (Both emissions to air from combustion and losses to the forest products sector.)")
    

df <- outputSummary %>%
    filter(Time >=1,
           mgmtID >= 10000 |
               mgmtID == 1,
           variable %in% variableLvl) %>%
    mutate(mgmtScenarioName = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    ##################
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
            
             replicate, Time, variable) %>%
    summarize(totalArea = sum(mgmtArea_ha),
              value = weighted.mean(value, mgmtArea_ha)) %>%
    ungroup() %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
             Time, variable) %>%
    summarise(value = mean(value))

    
if(a == "Hereford") {

    p <- ggplot(df, aes(x =  initYear+Time, y = value*unitConvFact, #group = simID,
                       colour = mgmtScenario)) 
}
if(a == "ForMont") {
    
    p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact, #group = simID,
                       colour = as.factor(mgmtScenarioName)))
}

p <- p + facet_grid(variable ~ scenario, scale = "free") +#facet_wrap( ~ scenario) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey25", size = 0.35) +
    geom_line() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(name = "Scénario\nd'aménagement",
                       values = cols[[a]]) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = paste("Dynamique du carbone"),
         subtitle = paste(areaName, simName),
         y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
         caption = paste(caption, collapse = "\n")) 


png(filename= paste0("fluxes_", simName, ".png"),
    width = 8, height = 8, units = "in", res = 600, pointsize=10)
print(p)

dev.off()

################################################################################
################################################################################
### to PFS
df <- fps %>%
    
    mutate(mgmtScenarioName = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%

    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,  
             Time, species) %>%
    summarise(BioToFPS_tonnesCTotal = mean(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))

dfTotal <- df %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,  
             Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))
    
labdf <- df %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName) %>%
    summarise(areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha)) 

yMax <- df %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName, Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) %>%
    group_by() %>%
    summarise(yMax = max(BioToFPS_tonnesCTotal))
yMax <- as.numeric(yMax)


require(RColorBrewer)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

### stacked (per species)
pWidth <- 4*length(unique(df$mgmtScenario))
png(filename= paste0("fps_spp_", simName, ".png"),
    width = pWidth, height = 6, units = "in", res = 600, pointsize=10)

#ggplot(df, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) + 
ggplot(df, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(scenario ~ mgmtScenarioName ) +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Transfers vers les produits forestiers",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes C récoltées","\n"))) +
    geom_text(data = labdf, aes(label = paste("Superficie aménagée:", areaManagedTotal_ha, "ha"),
                                y = yMax, x = initYear),
              hjust = 0, vjust = 1, size = 2)
    
dev.off()



### total
if(a == "Hereford") {
    p <- ggplot(dfTotal, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal,
                             colour = mgmtScenario))
}
if(a == "ForMont") {
    p <- ggplot(dfTotal, aes(x = initYear+Time, y = BioToFPS_tonnesCTotal,
                             colour = mgmtScenarioName))
}

p <- p + geom_line()+
    facet_wrap( ~ scenario) +
    scale_color_manual(name = "Scénario\nd'aménagement",
                       values = cols[[a]]) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    lims(y = c(0,20000)) +
    labs(title = "Transfers vers les produits forestiers",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes C récoltées","\n")))
         #y = expression(paste("tonnes C"," ha"^"-1", "récolté","\n")))


png(filename= paste0("fps_total_", simName, ".png"),
    width = 8, height = 4, units = "in", res = 600, pointsize=10)
print(p)

dev.off()




################################################################################
################################################################################
################################################################################
### Aboveground biomass
################################################################################
require(raster)
# 
# ### must correct this
# # 
# if(a == "ForMont") {
#     r <- raster("../inputsLandis/landtypes_ForMont_cropped.tif")
# }
# if(a == "Hereford") {
#     r <- raster("../inputsLandis/landtypes_Hereford_cropped.tif")
# }
# 
# # totalArea <- AGB %>%
# #     distinct(areaName, landtype, landtypeArea_ha)
# # totalArea <- sum(totalArea$landtypeArea_ha)
# 
# # 
# # foo <- filter(AGB,
# #               scenario == "baseline",
# #               mgmtScenario == "0",
# #               replicate == 1, Time == 10, species == "ABIE.BAL")
# 
# # sum(foo$agb_tonnesTotal)/totalArea



df <- AGB %>%
    mutate(mgmtScenarioName = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName, 
             Time, replicate,
             # ageClass,
             species) %>%
    summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
              areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
    group_by(areaName, scenario, mgmtScenario, mgmtScenarioName,
             #ageClass.
             Time, species) %>%
    summarise(agb_tonnesTotal = mean(agb_tonnesTotal),
              areaTotal_ha = unique(areaTotal_ha)) %>%
    as.data.frame()

require(RColorBrewer)

### stacked (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
pHeight <- 2 * length(unique(df$mgmtScenario))
png(filename= paste0("agb_sppStack_", simName, ".png"),
    width = 8, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenarioName ~ scenario) +
    scale_fill_manual("",
                      values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la composition forestière  - Biomasse aérienne*",
         subtitle = paste(areaName, simName),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")


dev.off()


### line (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

png(filename= paste0("agb_sppLine_", simName, ".png"),
    width = 8, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha,
               colour = species,
               group = species)) + 
    geom_line() +
    #stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenarioName ~ scenario) +
    scale_colour_manual("",
                        values = getPalette(colourCount)) +
    #scale_fill_manual(values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la composition forestière  - Biomasse aérienne*",
         subtitle = paste(areaName),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")


dev.off()
# 
# ################################################################################
# ### stacked (age classes)
# cols = brewer.pal(n = 9, name = 'Greens')[3:9]
# ################################################################################
# png(filename= paste0("agb_AgeClassStacked_", a, ".png"),
#     width = 10, height = 20, units = "in", res = 600, pointsize=10)
# 
# ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/totalManagedArea)) +
#     stat_summary(aes(fill = ageClass), fun.y="sum", geom="area", position = "stack") +
#     facet_grid(species ~ scenario, scales = "free_y") +
#     scale_fill_manual(values = cols)+
#     theme_dark() +
#     theme(plot.caption = element_text(size = rel(.5), hjust = 0),
#           axis.text.x = element_text(angle = 45, hjust = 1)) +
#     labs(title = "?volution de la composition forestière de la MRC Maskinong?\nBiomasse aérienne* par classes d'?ge",
#          x = "",
#          y = expression(paste("tonnes"," ha"^"-1")),
#          caption = "*Les valeurs sont exprim?es ici en terme de poids sec (biomasse), et non de carbone")
# 
# 
# dev.off()


################################################################################
################################################################################
################################################################################
### Aboveground biomass (by age classes)
################################################################################
####




# df <- AGB %>%
#     mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
#                                  levels = mgmtLevels[[a]])) %>%
#     mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
#                           ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
#                                  ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
#        mgmt = factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls)) %>%
#     group_by(areaName, scenario, mgmtScenario,
#              ##################
#              plantedSp, ageClass, mgmt,
#              Time, replicate,
#              # ageClass,
#              species) %>%
#     summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
#               areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
#     group_by(areaName, scenario, mgmtScenario,
#              ##################
#              plantedSp, ageClass, mgmt,
#              #ageClass.
#              Time, species) %>%
#     summarise(agb_tonnesTotal = mean(agb_tonnesTotal),
#               areaTotal_ha = unique(areaTotal_ha)) %>%
#     group_by(areaName, scenario, mgmtScenario,
#              ##################
#              plantedSp, ageClass, mgmt,
#              #ageClass.
#              Time) %>%
#     summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
#               areaTotal_ha = unique(areaTotal_ha)) %>%
#     as.data.frame()

##########################

df <- AGB %>%
    mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]]))

if (a == "ForMont") {
   df <- df %>% mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
                                          ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
                                                 ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
                       mgmt =  factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls))
}
if (a == "Hereford") {
    df <- df %>% mutate(mgmt =  mgmtScenario)
    
}



df <- df %>% 
    group_by(areaName, scenario, mgmtScenario,
             mgmt,
             Time, replicate,
             ageClass,
             species) %>%
    summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
              areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
    group_by(areaName, scenario, mgmt,
             ageClass,
             Time, species) %>%
    summarise(agb_tonnesTotal = mean(agb_tonnesTotal),
              areaTotal_ha = unique(areaTotal_ha)) %>%
    as.data.frame()


require(RColorBrewer)


################################################################################
### stacked (age classes, global)
cols = rev(brewer.pal(n = 9, name = 'Greens')[3:9])

# ################################################################################


acLvls <- levels(df$ageClass)
acLvls <- rev(acLvls)
df[,"ageClass"] <- factor(df$ageClass, levels = acLvls)


#### mgmt ~ scenario
    
p <- ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha)) + 
    stat_summary(aes(fill = ageClass), fun.y="sum", geom="area", position = "stack") +
    facet_grid(scenario ~ mgmt, scales = "fixed") +#scales = "free_y") +
    scale_fill_manual("Classe d'âge",
                      values = cols)+
    theme_bw() +
    theme(plot.caption = element_text(size = rel(1), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0("Évolution de la structure d'âge de la ", areaName),
         subtitle = paste0("Biomasse aérienne* par classes d'âge"),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")

png(filename= paste0("agb_AgeClassStacked_",simName, ".png"),
    width = 10, height = 6, units = "in", res = 600, pointsize=10)
    print(p)
dev.off()



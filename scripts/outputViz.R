###################################################################
###################################################################
### ForCS output processing 
rm(list = ls())
home <- path.expand("~")
home <- gsub("\\\\", "/", home)
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/", sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
require(dplyr)

initYear <- 2020
unitConvFact <- 0.01 ### from gC /m2 to tonnes per ha
a <- "ForMont"
areaName <- ifelse(a == "ForMont", "Forêt Montmorency",
                   ifelse(a == "Hereford", "Forêt Hereford", "[placeholder]"))
require(ggplot2)
require(dplyr)
require(tidyr)


##########################################################
##### pools
variableLvl <- c("TotalEcosys", "TotalDOM", "ABio", "BBio") ## ordering levels for plotting


mgmtLevels <- list(ForMont = c("0" = "Référence",
                               "noHarvest" = "Conservation totale",
                               "1" = "Extensif 1",
                               "2.1" = "Extensif 2 - EPN",
                               "2.2" = "Extensif 2 - EPB",
                               "2.3" = "Extensif 2 - EPR",
                               "3.1" = "Extensif 3 - EPN",
                               "3.2" = "Extensif 3 - EPB",
                               "3.3" = "Extensif 3 - EPR",
                               "4.1" = "Intensif - EPN",
                               "4.2" = "Intensif - EPB",
                               "4.3" = "Intensif - EPR"),
                   Hereford = c("3" = "Référence",
                                "noHarvest" = "Conservation",
                                "4" = "Nouveau zonage",
                                "2" = "Allongement",
                                "1" = "Intensif"))

mgmtLvls <- c("Référence", "Conservation totale", "Extensif 1", "Extensif 2", "Extensif 3", "Intensif")




cols <- list(ForMont = c("Référence" = "black",
                         "Conservation totale" = "darkgreen",
                         "Extensif 1" = "green",
                         "Extensif 2" = "dodgerblue3",
                         "Extensif 3" = "goldenrod3",
                         "Intensif" = "red3"),
             Hereford =  c("Nouveau zonage" = "dodgerblue3",
                           "Conservation" = "darkolivegreen3",
                           "Servitude" = "black",
                           "Allongement" = "goldenrod3",
                           "Intensif" = "red3"))

scenRef <- list(ForMont = "Référence",
                Hereford = "Référence")
################################################################################
################################################################################
################################################################################
outputSummary <- get(load(paste0("../outputCompiled/output_summary_", a, ".RData")))
fps <- read.csv(paste0("../outputCompiled/output_BioToFPS_", a, ".csv"))#read.csv("output_BioToFPS_Hereford.csv")#
AGB <- get(load(paste0("../outputCompiled/output_bio_", a, ".RData")))

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
    mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    ##################
    mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
                       ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
                              ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
    mgmt = factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls)) %>%
        
    filter(Time >=1,
           mgmtID >= 10000 |
               mgmtID == 1,
           variable %in% variableLvl) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             mgmtID, Time, variable) %>%
    summarise(value = mean(value),
              mgmtArea_ha = unique(mgmtArea_ha)) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             Time, variable) %>%
    summarise(valueTotal = sum(value*mgmtArea_ha),
              mgmtArea_ha = sum(mgmtArea_ha)) %>%
    mutate(value = valueTotal/mgmtArea_ha) %>%
    as.data.frame()


df <- df %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
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
           mgmt == scenRef[[a]])
cNames <- colnames(df)
cNames <- cNames[-which(cNames %in% c("mgmtScenario", "mgmt", "valueTotal", "value", "mgmtArea_ha"))]

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
                   colour = mgmt,
                   linetype = plantedSp))
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
         subtitle = areaName,
         x = "",
         y = expression(paste("tonnes C"," ha"^"-1","\n")),
         caption = paste0("ABio : Biomasse aérienne",
                          "\nBBio : Biomasse souterraine",
                          "\nTotalDOM : Bois mort, litière, humus et sol minéral"))

if(a == "ForMont") {
    p <- p +
        scale_linetype_manual(name = "Espèce plantée",
                              values = c(1, 2, 3, 4)) 
}
    
png(filename= paste0("pools_", a, ".png"),
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
    mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    ##################
    mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
                              ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
                                     ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
           mgmt = factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls)) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             replicate, Time, variable) %>%
    summarize(totalArea = sum(mgmtArea_ha),
              value = weighted.mean(value, mgmtArea_ha)) %>%
    ungroup() %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             Time, variable) %>%
    summarise(value = mean(value))

    
if(a == "Hereford") {

    p <- ggplot(df, aes(x =  initYear+Time, y = value*unitConvFact, #group = simID,
                       colour = mgmtScenario)) 
}
if(a == "ForMont") {
    
    p <- ggplot(df, aes(x = initYear+Time, y = value*unitConvFact, #group = simID,
                       colour = as.factor(mgmt),
                       linetype = plantedSp))
}

p <- p + facet_grid(variable ~ scenario, scale = "free") +#facet_wrap( ~ scenario) +
    theme_bw() +
    geom_hline(yintercept = 0, linetype = 1, color = "grey25", size = 0.35) +
    geom_line() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(name = "Scénario\nd'aménagement",
                       values = cols[[a]]) +
    scale_linetype_manual(name = "Espèce plantée",
                          values = 1:4) +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0)) +
    labs(title = paste("Dynamique du carbone"),
         subtitle = areaName,
         y = expression(paste("tonnes C"," ha"^"-1", " yr"^"-1", "\n")),
         caption = paste(caption, collapse = "\n")) 


png(filename= paste0("fluxes_", a, ".png"),
    width = 8, height = 8, units = "in", res = 600, pointsize=10)
print(p)

dev.off()

################################################################################
################################################################################
### to PFS
df <- fps %>%
    
    mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    ##################
    mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
                              ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
                                     ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
           mgmt = factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls)) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             Time, species) %>%
    summarise(BioToFPS_tonnesCTotal = mean(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))

dfTotal <- df %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal),
              areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha))
    
labdf <- df %>%
    group_by(areaName, scenario, mgmtScenario) %>%
    summarise(areaManagedTotal_ha = unique(areaManagedTotal_ha),
              areaHarvestedTotal_ha = mean(areaHarvestedTotal_ha)) 

yMax <- df %>%
    group_by(areaName, scenario, mgmtScenario, Time) %>%
    summarise(BioToFPS_tonnesCTotal = sum(BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) %>%
    group_by() %>%
    summarise(yMax = max(BioToFPS_tonnesCTotal))
yMax <- as.numeric(yMax)


require(RColorBrewer)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(8, "Set1"))

### stacked (per species)
pHeight <- 1.5*length(levels(df$mgmtScenario))
png(filename= paste0("fps_spp_", a, ".png"),
    width = 8, height = pHeight, units = "in", res = 600, pointsize=10)

#ggplot(df, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal/areaHarvestedTotal_ha)) + 
ggplot(df, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenario ~ scenario) +
    scale_fill_manual(values = getPalette(colourCount)) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Transfers vers les produits forestiers",
         subtitle = areaName,
         x = "",
         y = expression(paste("tonnes C récoltées","\n"))) +
    geom_text(data = labdf, aes(label = paste("Superficie aménagée:", areaManagedTotal_ha, "ha"),
                                y = yMax, x = 2010),
              hjust = 0, vjust = 1, size = 2)
    
dev.off()



### total
if(a == "Hereford") {
    p <- ggplot(dfTotal, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal,
                             colour = mgmtScenario))
}
if(a == "ForMont") {
    p <- ggplot(dfTotal, aes(x = 2010+Time, y = BioToFPS_tonnesCTotal,
                             colour = mgmt,
                             linetype = plantedSp))
}

p <- p + geom_line()+
    facet_wrap( ~ scenario) +
    scale_color_manual(name = "Scénario\nd'aménagement",
                       values = cols[[a]]) +
    theme_dark() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Transfers vers les produits forestiers",
         subtitle = areaName,
         x = "",
         y = expression(paste("tonnes C récoltées","\n")))
         #y = expression(paste("tonnes C"," ha"^"-1", "récolté","\n")))


png(filename= paste0("fps_total_", a, ".png"),
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
# 
if(a == "ForMont") {
    r <- raster("../inputsLandis/landtypes_ForMont_cropped.tif")
}
if(a == "Hereford") {
    r <- raster("../inputsLandis/landtypes_Hereford_cropped.tif")
}
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
    mutate(mgmtScenario = factor(mgmtLevels[[a]][match(as.character(mgmtScenario), names(mgmtLevels[[a]]))],
                                 levels = mgmtLevels[[a]])) %>%
    #################
    mutate(plantedSp = ifelse(grepl("EPB", mgmtScenario), "P. glauca",
                          ifelse(grepl( "EPN", mgmtScenario), "P. mariana",
                                 ifelse(grepl("EPR", mgmtScenario), "P. rubens", "N/A"))),
           mgmt = factor(gsub(" - |EPN|EPB|EPR", "", mgmtScenario), levels = mgmtLvls)) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
             Time, replicate,
             # ageClass,
             species) %>%
    summarise(agb_tonnesTotal = sum(agb_tonnesTotal),
              areaTotal_ha = sum(unique(landtypeArea_ha))) %>%
    group_by(areaName, scenario, mgmtScenario,
             ##################
             plantedSp, mgmt,
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
png(filename= paste0("agb_sppStack_", a, ".png"),
    width = 8, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha)) + 
    stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenario ~ scenario) +
    scale_fill_manual("",
                      values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la composition forestière  - Biomasse aérienne*",
         subtitle = paste(areaName, " (incluant les environs)"),
         x = "",
         y = expression(paste("tonnes"," ha"^"-1")),
         caption = "*Les valeurs sont exprimées ici en terme de poids sec (biomasse), et non de carbone")


dev.off()


### line (total)
colourCount = length(unique(df$species))
getPalette = colorRampPalette(brewer.pal(9, "Set1"))

png(filename= paste0("agb_sppLine_", a, ".png"),
    width = 8, height = pHeight, units = "in", res = 600, pointsize=10)

ggplot(df, aes(x = 2020+Time, y = agb_tonnesTotal/areaTotal_ha,
               colour = species,
               group = species)) + 
    geom_line() +
    #stat_summary(aes(fill = species), fun.y="sum", geom="area", position = "stack") +
    facet_grid(mgmtScenario ~ scenario) +
    scale_colour_manual("",
                        values = getPalette(colourCount)) +
    #scale_fill_manual(values = getPalette(colourCount)) +
    theme_bw() +
    theme(plot.caption = element_text(size = rel(.5), hjust = 0),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Évolution de la composition forestière  - Biomasse aérienne*",
         subtitle = paste(areaName, " (incluant les environs)"),
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

png(filename= paste0("agb_AgeClassStacked_",a, ".png"),
    width = 10, height = 6, units = "in", res = 600, pointsize=10)
    print(p)
dev.off()



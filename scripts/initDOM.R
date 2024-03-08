################################################################################
################################################################################
### Some code to produce a area extent within which we fetch NFI plots for 
### study area calibration
### Dominic Cyr
#############
rm(list = ls())
home <- path.expand("~")
home <- gsub("/Documents", "", home) # necessary on my Windows machine
setwd(paste(home, "Sync/Travail/ECCC/Landis-II/frqnt_2022-25", sep ="/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(rgeos)
require(tidyverse)
a <- "mixedwood-042-51"#c("Hereford", "Maskinonge", "ForMont")

df <- read.csv("0/log_Pools.csv")#/log_Pools.csv")

# 1 "Very Fast Aboveground"
# 2 "Very Fast Belowground"
# 3 "Fast Aboveground"
# 4 "Fast Belowground"
# 5 "Medium"
# 6 "Slow Aboveground"
# 7 "Slow Belowground"
# 8 "Stem Snag"
# 9 "Other Snag"
# 10 "Extra pool"


################################################################################
################################################################################
#### Assing DOM values based on ForCS spinup

domPools <- c(VF_A = 1,
              VF_B = 2,
              Fast_A = 3,
              Fast_B = 4,
              MED = 5,
              Slow_A = 6,
              Slow_B = 7,
              Sng_Stem = 8,
              Sng_Oth = 9,
              Extra = 10)

x <- df %>%
  filter(Time == 0) %>%
  group_by(ecoregion, species) %>%
  summarise(VF_A = mean(VF_A),
            VF_B = mean(VF_B),
            Fast_A = mean(Fast_A),
            Fast_B = mean(Fast_B),
            MED = mean(MED),
            Slow_A = mean(Slow_A),
            Slow_B = mean(Slow_B),
            Sng_Stem = mean(Sng_Stem),
            Sng_Oth = mean(Sng_Oth),
            Extra = mean(Extra)) %>%
  gather(key = "pool", value = "amountAtT0", -ecoregion, -species) %>%
  mutate(landtype = ecoregion,
         spp = species,
         poolID = domPools[match(pool, names(domPools))],
         amountAtT0 = round(amountAtT0)) %>%
  ungroup() %>%
  select(landtype, spp, poolID, amountAtT0) %>%
  arrange(spp, landtype, poolID)
  
write.csv(x, file = paste0("DOM-initPools_", a, ".csv"), row.names = F)


# df <- DOMPool_fetch(x = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/2020-03-03/0/log_Pools.csv",
#               ltTxt = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/2020-03-03/0/landtypes.txt")
# write.csv(df, file = "DOM-initPools_Hereford.csv", row.names = F)
df <- DOMPool_fetch(x = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/2021-07-21/0/log_Pools.csv",
                    ltTxt = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/2021-07-21/0/landtypes.txt")
write.csv(df, file = "DOM-initPools_ForMont.csv", row.names = F)
# df <- DOMPool_fetch(x = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/2020-02-26/2/log_Pools.csv",
#                     ltTxt = "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/2020-02-26/2/landtypes.txt")
# write.csv(df, file = "DOM-initPools_Maskinonge.csv", row.names = F)
# df %>% arrange(spp, landtype,  poolID)
DOMPool_fetch <- function(x = "log_Pools.csv",
                          ltTxt = "landtypes.txt") {
    
    
    
    require(dplyr)
    require(tidyr)
    domPool <- read.csv(x)
    #
    domNames <- colnames(domPool)[6:15]
    domNames <- factor(domNames, levels = domNames)

                  
    
    lt <- read.table(ltTxt, skip = 1, comment.char = ">")
    lt <- lt[which(lt$V1 %in% c("yes", "y", "Yes","Y", 1)),2]
    lt <- data.frame(landtypeName = lt,
                     landtype = 1:length(lt)) %>%
        slice(rep(row_number(), length(domNames))) %>%
        arrange(landtype)
    lt[, "DOMPool"] <- rep(as.numeric(domNames), length(unique(lt$landtype)))

    df <- domPool %>%
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
        gather(key = "DOMName", value = "amountAtT0", -ecoregion, -species)
    
    
    ### adding zero values (would probably not work if initial communities don't include all species)
    tmp <- expand.grid(ecoregion = unique(df$ecoregion),
                       species = unique(df$species),
                       DOMName = unique(df$DOMName))
    df <- tmp %>%
      merge(df, all.x = T)
    
    df[is.na(df$amountAtT0), "amountAtT0"] <- 0
    
    ### naming landtypes
    #df[,"landtype"] <- lt[match(df$ecoregion, lt$landtype), "landtypeName"]
    
    
    ### cleaning up
    df <- data.frame(landtype = as.factor(df$ecoregion),
                     spp = df$species,
                     #DOMName = df$DOMName,
                     poolID = as.factor(match(df$DOMName, domNames)),
                     amountAtT0 = round(df$amountAtT0))
    
    df <- df %>%
        arrange(spp, landtype, poolID)
    
    return(df)
    
}



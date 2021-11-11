wwd <- ifelse(Sys.info()["sysname"] == "Linux",
              paste(home, "Sync/Travail/ECCC/Landis-II/Montmorency_calib/", sep = "/"),
              "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib/")
wwd <- paste(wwd, Sys.Date(), sep= "/")
dir.create(wwd)
setwd(wwd)
require(ggplot2)
require(tidyverse)


for (s in c("ForMont_gs-ms")) {
    outLandis <- read.csv(paste0("../outputCompiled/AGbio_output_",s,".csv"))
    out3PG <-  read.csv("../2021-08-02/Biomass_test.csv", sep = ";")
    
    
    
    outLandis <- outLandis %>%
        group_by(initComm, growthShape) %>%
        mutate(maxVal = max(ABio_C_TonnesPerHa),
               minVal = min(ABio_C_TonnesPerHa),
               ABio_C_scaled = (ABio_C_TonnesPerHa-minVal)/(maxVal-minVal)) %>%
        ungroup()
    
    
    out3PG <- out3PG %>%
        mutate(Time = Stand.age,
               ABio_C_TonnesPerHa = Stem_DM_TonneHa *.5,
               treatment = Traitement,
               initComm = ifelse(Essence == "SAB", "ABIE.BAL", "PICE.GLA")) %>%
        filter(treatment == "T?moin") %>%
        group_by(initComm) %>%
        mutate(maxVal = max(ABio_C_TonnesPerHa),
               minVal = min(ABio_C_TonnesPerHa),
               ABio_C_scaled = (ABio_C_TonnesPerHa-minVal)/(maxVal-minVal)) %>%
        ungroup()
    
    
    df <- outLandis %>%
        filter(treatment == "CPRS")
    
    # ggplot(df, aes(x = Time, y = ABio_C_TonnesPerHa)) +
    #     facet_grid( ~initComm) +
    #     geom_line(aes(colour = as.factor(growthShape))) +
    #     geom_line(data = out3PG, aes(x = Time, y = ABio_C_TonnesPerHa))
    
    p <- ggplot(df, aes(x = Time, y = ABio_C_scaled)) +
        facet_grid( ~initComm) +
        geom_line(aes(colour = as.factor(growthShape))) +
        geom_line(data = out3PG, aes(x = Time, y = ABio_C_scaled), size = 2)
    
    png(filename = paste0("agbCalib_", s,".png"),units = "in", res = 300,
        width = 12, height = 5)
    
    print(p)

    dev.off()
}


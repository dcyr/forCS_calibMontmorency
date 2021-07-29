#rm(list =ls())
# wwd <- setwd("D:/Landis_ForCS_test")
# wwd <- "C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency-Hereford/2019-09-24"
# setwd(wwd)
wwd <- getwd()

simInfo <- read.csv("simInfo.csv", colClasses = c(simID = "character"))
simDir <- simInfo$simID

require(parallel)
require(doSNOW)
n <- ifelse(Sys.info()["user"] == "dcyr-z840",
            12 , 1)

n <- min(n, nrow(simInfo))

# #######
cl = makeCluster(n, outfile = "") ## 
registerDoSNOW(cl)

foreach(i = 1:length(simDir)) %dopar% { 

    setwd(paste(wwd, simInfo[i, "simID"], sep = "/"))
    
    readmeOld <- readLines("README.txt")
    readmeOld <- readmeOld[1:which(grepl(tail(colnames(simInfo), 1), readmeOld))]
    
    x <- as.character(shell("landis-ii-extensions.cmd list", intern = T))
    
    sink(file = "README.txt")
    
    for(l in seq_along(readmeOld)) {
        cat(readmeOld[l])
        cat("\n") 
    }
    
    sink()
    
    sink(file = "README.txt", append = T)
    cat("\n")
    cat("#######################################################################\n")
    cat("########### Installed LANDIS-II extensions\n")
    cat("#######################################################################\n")
    for (l in seq_along(x)) {
        cat(x[l])
        cat("\n")   
    }
    cat("\n")
    cat("#######################################################################\n")
    cat("########### System Info\n")
    cat(write.table(as.data.frame(Sys.info()),
                    quote = F, col.names = F))
    sink()
    
    shell("landis-ii-7.cmd scenario.txt", wait = T)
}

stopCluster(cl)

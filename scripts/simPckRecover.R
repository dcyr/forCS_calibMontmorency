### a script to revover LANDIS-II / ForCS simPck a posteriori for reproducibility
setwd("C:/Users/dcyr-z840/Sync/Travail/ECCC/Landis-II/Montmorency_calib")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)



simDir <- c("D:/ForMont_test-entireRange/",
            "D:/ForMont_test-entireRange-noRecruitment/")

for (s in simDir) {
    sOut <- basename(s)
    dir.create(sOut)
    
    
    
    ### creating the folder structure first
    d <- list.dirs(s, recursive = F)
    x <- gsub(s, getwd(), d)
    # removing the output folders
    
    for(i in x) {
        dir.create(paste(sOut, basename(i), sep = "/"))
    }
    ###
    
    ### copy root files
    x <- list.files(s, full.names = F)
    x <- x[!x %in% basename(d)]
    file.copy(paste(s, x, sep = "/"),
              paste(sOut,x, sep = "/"),
              overwrite = T)
    
    
    ### copy other files
    f <- list.files(d,
                    full.names = T,
                    include.dirs = F,
                    recursive = F)
    
    
    
    
    f <- f[!grepl("log", f)]
    f <- f[!basename(f) %in% c("bda", "harvest",  "fire", "Metadata", "wind")]
    
    x <- gsub(s, sOut, f)
    
    file.copy(f, x,
              overwrite = T,
              copy.date = T)
}



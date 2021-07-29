# lt <- read.table("./0/landtypes.txt", skip = 1, comment.char = ">")[-1,2]
# 
# 
# 
# WindEventParameters <- data.frame(landtype = lt,
#                                   maxSize = 1000,
#                                   meanSize = 25,
#                                   minSize = 6.25,
#                                   rotationPeriod = 2500)
# 
# 
# WindSeverities <- data.frame(Severity = 5:1,
#                              AgeClass = c("0% to 20%",
#                                           "20% to 40%",
#                                           "50% to 70%",
#                                           "70% to 85%",
#                                           "85% to 100%"),
#                              Prob = c(0.05,
#                                       0.1,
#                                       0.5,
#                                       0.85,
#                                       0.95))

# 
# initBaseWind(outFile = 'base-wind_ForMont.txt',
#              WindEventParameters = WindEventParameters,
#              WindSeverities = WindSeverities)


initBaseWind <-  function(outFile = 'base-wind.txt',
                          Timestep = 1,
                          WindEventParameters,
                          WindSeverities) {
    
    ## intro section
    sink(outFile)
    
    cat('LandisData  "Base Wind"\n')
    cat("\n")
    cat(paste("Timestep", Timestep, "\n"))
    cat("\n")
    cat(">>Wind Event Parameters\n")
    cat(paste(">>", paste(colnames(WindEventParameters), collapse = "\t"), "\n"))
    
    sink()
    
   
    write.table(WindEventParameters, file = outFile,
                append = T,
                sep = "\t",
                quote = F,
                col.names = F,
                row.names = F,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\n") #default line endings on windows system.)
    
    
    sink(outFile, append = T)
    cat("\n")
    cat("WindSeverities\n")
    cat(paste(">>", paste(colnames(WindSeverities), collapse = "\t"), "\n"))
    write.table(WindSeverities, file = outFile,
                append = T,
                sep = "\t",
                quote = F,
                col.names = F,
                row.names = F,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\n") #default line endings on windows system.)
    
    sink(outFile, append = T)
    cat("\n")
    cat("MapNames wind/severity-{timestep}.tif\n")
    cat("LogFile wind/log.csv\n")
    sink()
}
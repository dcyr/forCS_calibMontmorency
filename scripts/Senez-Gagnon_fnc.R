


tsd <- 1:80

treeBiomass_fnc <- function(x) {## Chapman-Richards
  trees <- 77.124*(1-exp(-0.043*x))^3.294
  return(trees) ### in Mg of C per ha
}

# treeBiomass <- treeBiomass_fnc(tsd)
# 
# 
totalAGB_fcn <- function(x) {## Chapman-Richards
  agb <- 94.947*exp(-exp(-0.039*(x-23.08)))
  return(agb) ### in Mg of C per ha
}
# 
# agb <- totalAGB_fcn(tsd)
# 
# plot(x = tsd,
#      y = agb,
#      type = "l",
#      ylim = c(0,90))
# 
# lines(x= tsd,
#      y = treeBiomass)



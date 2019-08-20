## install many packages at a go
packagesLoad <-c("R2OpenBUGS","tidyverse","rgeos",
                 "maptools","broom","rgdal","rgeos",
                 "mcmc","spdep","data.table","haven",
                 "PerformanceAnalytics")
for(i in 1:length(packagesLoad)){
  if(require(packagesLoad[i],
             character.only = TRUE)==FALSE){
    install.packages(packagesLoad[i]);
  }}


##install INLA for later
install.packages("INLA", repos=c(getOption("repos"),
                                 INLA="https://inla.r-inla-download.org/R/stable"),
                 dep=TRUE)

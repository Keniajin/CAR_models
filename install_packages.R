## install many packages at a go
install.packages("ggplot")
install.packages("mcmcplots")
install.packages("readstata13")
install.packages("callr")
install.packages("dplyr")
install.packages("R2WinBUGS")
packagesLoad <-c("R2OpenBUGS","dplyr","rgeos",
                 "maptools","broom","rgdal","rgeos",
                 "mcmc","spdep","data.table")
for(i in 1:length(packagesLoad)){
  if(require(packagesLoad[i],
             character.only = TRUE)==FALSE){
    install.packages(packagesLoad[i]);
  }}


##install INLA for later
install.packages("INLA", repos=c(getOption("repos"),
                                 INLA="https://inla.r-inla-download.org/R/stable"),
                 dep=TRUE)

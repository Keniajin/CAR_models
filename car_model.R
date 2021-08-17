### install all packages we need 
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
##install INLA 
install.packages("INLA", repos=c(getOption("repos"),
    INLA="https://inla.r-inla-download.org/R/stable"),
                 dep=TRUE)


## load the packages needed for this module
library(R2OpenBUGS) ## Help in connecting R to WinBugs
library(dplyr) ## package for data manipulation
library(rgeos) ## Geomegry Engine- Open Source (GEOS)
library(maptools) ##  R package with useful map tools
library(broom) ## for converting a map to a data frame
library(rgdal) ## Geospatial Data Analysis Library (GDAL)"
library(rgeos) # # "Geomegry Engine- Open Source (GEOS)" -- check your shape file
library(mcmc)
library(spdep)
library(data.table)
library("PerformanceAnalytics") ## EDA
library(INLA) ## inla
library(ggplot2)
library(readstata13)

## remove the data in the environment
rm(list = ls())

## set working directory to  the folder where you downloaded the files
## If you open the project folder you dont need to set the working directory
##uncomment to  
#setwd("H:/courses/WIts/CAR_model/CAR_in_R")

###reading and exporting the shape file
## what are comple files of a shape file??? --> dbf file , prj file, shp file , shx
zim_shp <- maptools::readShapePoly("ZWE_adm2.shp",
                                        IDvar="ID_2")
plot(zim_shp, border="red", axes=TRUE, las=1 )

## what R is reccommending 
zim_OGR <-  rgdal::readOGR("ZWE_adm2.shp")

plot(zim_OGR, border="blue", axes=TRUE, las=1)

#Our first check should be to see if the spatial geometry is valid– 
#the gIsValid function looks at each polygon, and makes sure it doesn’t 
#break any topological rules. Ideally, all of the geometry is valid
#, and the total number of polygons that fail the validity tests is 0.
zim_valid <- data.table(valid=gIsValid(zim_OGR,byid=TRUE))
sum(zim_valid$valid==F)

## export a file to use in WinBugs called Splus
## example of splus map in Open Bugs
###then open wibug as a text file import the map
## sp2WB - the function exports an sp SpatialPolygons object into a 
## S-Plus map format to be import by WinBUGS.
maptools::sp2WB(map =as(zim_shp, "SpatialPolygons"),
                filename="zim_splus" )

## get the adjacency matrix for the map
## ### adjacency matrix from Arcmap using Adjacency for Winbugs Addon -- how you can get it in bugs
##Construct neighbours list from polygon list
zim_nb<- spdep::poly2nb(zim_shp)
num <- sapply(zim_nb, length)
adj <- unlist(zim_nb)
sumNumNeigh <- length(unlist(zim_nb))

## load the data set of interest
#zim_child_data <- readstata13::read.dta13("Data2_old.dta")
zim_child_data <- haven::read_dta("Data2.dta")

dplyr::glimpse(zim_child_data)

## EDA
eda_data <- zim_child_data %>% 
  select(Stunting, Employed, b19, Education, b4 , v025, BMI)
## works on R version 3.6.1
#PerformanceAnalytics::chart.Correlation(eda_data, histogram=TRUE, pch=19)

## fit 
form_fit <- Stunting ~ Employed +b19  +  as.factor(Education) + b4+ v025+ BMI
lin_mod <- glm(form_fit, 
               data=zim_child_data, 
               family=gaussian(link="identity"))

summary(lin_mod)
summary(lin_mod)$coefficient

AIC(lin_mod)  # AIC => 17272
BIC(lin_mod)  # BIC => 17333
##Residuals vs Fitted values
##Normal Q-Q
par(mfrow = c(2,2)) # display a unique layout for all graphs
plot(lin_mod)
par(mfrow = c(1,1)) 

## lets go to BUGS to fit a CAR model
## select the variables of interest
zim_child_model <- zim_child_data %>%  
  select(id_2 , name_1 , name_2 , Stunting,
         Employed, b19, Education, b4, v025, BMI)
zim_child_model <- zim_child_model[complete.cases(zim_child_model),]

## check whether the ID_2 on the shape file corresponds to what you have in your data
table(zim_shp@data$ID_2 %in% zim_child_model$id_2)
table(zim_shp@data$NAME_2 %in% zim_child_model$name_1)


###modelling 
## Define the number of neighbours
num = c(5, 7, 8, 4, 4, 8, 5, 3, 4, 7, 
        5, 5, 8, 6, 4, 8, 8, 8, 7, 4, 
        7, 5, 8, 7, 6, 6, 5, 10, 3, 6, 
        7, 7, 7, 6, 6, 8, 7, 4, 5, 8, 
        4, 7, 6, 4, 8, 3, 6, 6, 9, 2, 
        6, 6, 7, 6, 6, 6, 6, 5, 6, 5)
## define the adjacent polygons 
adj = c(
  52, 51, 47, 45, 40, 
  31, 26, 23, 19, 18, 13, 10, 
  35, 32, 25, 17, 7, 6, 5, 4, 
  32, 7, 5, 3, 
  33, 32, 4, 3, 
  25, 22, 21, 19, 9, 8, 7, 3, 
  25, 8, 6, 4, 3, 
  9, 7, 6, 
  22, 20, 8, 6, 
  21, 18, 16, 14, 13, 11, 2, 
  16, 14, 13, 12, 10, 
  31, 30, 27, 13, 11, 
  31, 18, 16, 14, 12, 11, 10, 2, 
  24, 16, 15, 13, 11, 10, 
  24, 20, 16, 14, 
  24, 21, 18, 15, 14, 13, 11, 10, 
  53, 35, 28, 26, 25, 23, 19, 3, 
  24, 23, 21, 19, 16, 13, 10, 2, 
  25, 23, 21, 18, 17, 6, 2, 
  24, 22, 15, 9, 
  24, 22, 19, 18, 16, 10, 6, 
  24, 21, 20, 9, 6, 
  31, 28, 26, 25, 19, 18, 17, 2, 
  22, 21, 20, 18, 16, 15, 14, 
  23, 19, 17, 7, 6, 3, 
  31, 30, 28, 23, 17, 2, 
  54, 30, 29, 28, 12, 
  57, 55, 54, 53, 31, 30, 27, 26, 23, 17, 
  54, 39, 27, 
  54, 31, 28, 27, 26, 12, 
  30, 28, 26, 23, 13, 12, 2, 
  38, 36, 35, 33, 5, 4, 3, 
  46, 38, 37, 36, 34, 32, 5, 
  60, 59, 58, 37, 36, 33, 
  53, 38, 36, 32, 17, 3, 
  59, 53, 38, 37, 35, 34, 33, 32, 
  60, 58, 48, 46, 36, 34, 33, 
  36, 35, 33, 32, 
  55, 54, 42, 41, 29, 
  57, 56, 52, 49, 45, 43, 42, 1, 
  47, 44, 42, 39, 
  55, 45, 44, 43, 41, 40, 39, 
  57, 56, 55, 49, 42, 40, 
  47, 45, 42, 41, 
  52, 51, 49, 47, 44, 42, 40, 1, 
  48, 37, 33, 
  51, 50, 45, 44, 41, 1, 
  58, 52, 51, 49, 46, 37, 
  60, 59, 58, 56, 52, 48, 45, 43, 40, 
  51, 47, 
  52, 50, 48, 47, 45, 1, 
  51, 49, 48, 45, 40, 1, 
  59, 57, 56, 36, 35, 28, 17, 
  55, 39, 30, 29, 28, 27, 
  57, 54, 43, 42, 39, 28, 
  59, 57, 53, 49, 43, 40, 
  56, 55, 53, 43, 40, 28, 
  60, 49, 48, 37, 34, 
  60, 56, 53, 49, 36, 34, 
  59, 58, 49, 37, 34)
## total neighbours
sumNumNeigh = 360

## create the data model to pass to winbugs
data_model <- list("N"=length(zim_child_model$Stunting) , 
                   "adj"=adj,"num"=num ,
                  "sumNumNeigh" =sumNumNeigh,
                  "Stunting" =zim_child_model$Stunting,
                  "Employed"=as.numeric(zim_child_model$Employed) ,
                  "Education"=zim_child_model$Education,
                  "b4" = as.numeric(zim_child_model$b4),
                  "v025" = as.numeric(zim_child_model$v025),
                 "BMI" = as.numeric(zim_child_model$BMI),
                  "b19"=as.numeric(zim_child_model$b19), 
                  "id_2"=zim_child_model$id_2)
names(data_model)

## set the number of decimal places

options("scipen"=100, "digits"=4)

### 
####defining the initials
# Initial Inits
params <- c("beta" , "Phi")
inits_Vals <-  function(){
  list(beta = c(0,0,0,0,0,0,0,0))
}


## run the bugs model and save the results in model_bugs
## the code below runs with openBugs
model_bugs <-  R2OpenBUGS::bugs(data = data_model,
                                model.file="bugs_models.txt",
                                inits=inits_Vals,
                                parameters.to.save = params,
                                n.chains=1,n.iter=10000,
                                n.burnin=2000,n.thin=10,codaPkg=T,digits = 5,
                                debug = F,DIC=TRUE,clearWD = TRUE,
                                ## uncomment below for WinBUGs
                                ##bugs.dir = "C:/Program Files (x86)/WinBUGS14/",
                                working.directory = file.path(getwd()))

### read the saved coda
coda_res   <- R2OpenBUGS::read.bugs("CODAchain1.txt")

##
# Get the posterior means of the model parameters
summary_model <- summary(coda_res)
summary_model$statistics

## diagnostics plots
## creat the folder to save the diagnostic plots
dir.create("results/figures", recursive = T)
mcmcplots::mcmcplot(coda_res, 
                    dir=file.path(getwd(),"results/figures") ,
                    regex = "beta|Phi")

## add the mean to a plot
df_model <- as.data.frame(summary_model$statistics) %>% 
  tibble::rownames_to_column("id_var")
glimpse(df_model)

## select spatial parameters only
df_model_spat <- df_model %>% 
  ##remain with Phi var
  filter(grepl("Phi",id_var)) %>% 
  mutate(ID_2=gsub("Phi\\[","",id_var),
         ID_2=gsub("\\]","",ID_2))


## plot the mean estimates on the map
zim_shp@data <- zim_shp@data  
zim_shp_df <- broom::tidy(zim_shp, region = "ID_2")
zim_shp_df <- zim_shp_df %>% 
  left_join(df_model_spat, by=c("id"="ID_2"))

glimpse(zim_shp_df)
p1 <- ggplot() + 
  geom_polygon(data = zim_shp_df, aes(x = long, y = lat,
                                  group = group,
                                  fill = Mean), colour = "white") + 
  theme_void()
p1


## save the file
ggpubr::ggexport("p1_bugs.png",p1)




#######################INLA Model##############################
## fit the model in INLA
## It uses the Integrated Nested Laplace Approximation, 
#a deterministic Bayesian method

form_fit <-  Stunting ~1+ Employed +b19  + 
  as.factor(Education) + b4+ v025+ BMI

#where 1 means that the model includes the intercept and
model_linear <- inla(form_fit,family="gaussian",data=zim_child_model,
                      control.compute=list(dic=TRUE, waic=TRUE))

summary(model_linear)
round(model_linear$summary.fixed[,1:5],3)

## Posterior density plot for the intercepts
plot(model_linear$marginals.fixed[[1]],
     type="l",
     main="",
     ylab="",
     xlab=expression(beta[0]))
plot(model_linear$marginals.fixed[[2]],
     type="l",
     main="",
     ylab="",
     xlab=expression(beta[1]))

## Spatial Model
## convert our neighbour map to an inla intergrated map
spdep::nb2INLA("zim_inla.graph", zim_nb)
zim_adj <- paste(getwd(),"/zim_inla.graph", sep="")


## Adjacency matrix for the ZIM example: rows and columns
## identify areas; squares identify neighbors
## plot the neigbour map
H <- inla.read.graph(filename="zim_inla.graph")
image(inla.graph2matrix(H),xlab="",ylab="")

### After having defined the neighborhood structure, 
## we need to specify the formula for the model, through
formula_inla <- Stunting ~ 1 + as.factor(Employed) +
  b19  + as.factor(Education) + b4+ v025+ BMI+
  f(id_2, model="bym",graph=zim_adj, scale.model=TRUE,
                     hyper=list(prec.unstruct=list(prior="loggamma",param=c(1,0.001)), 
                                prec.spatial=list(prior="loggamma",param=c(1,0.001))))


model_inla <- inla(formula_inla,family="gaussian",
                     data=zim_child_model,
                     control.compute=list(dic=TRUE))
 
summary(model_inla) #inla --> DIC 17218.91(pd =15.09) ##linear model--> DIC 17272.83(pd = 9.444)


## summary of the fixed effect
round(model_inla$summary.fixed,3)


##summary of the random effects
## top 60
round(head(model_inla$summary.random$id_2),3) 

####The   computation of the posterior mean for the random effects 𝝃 is performed in two
# steps as we have more than one parameter:
# we extract the marginal posterior distribution for each element of the random effect
csi <- model_inla$marginals.random$id_2[1:60]

# *** Code for posterior probablity
a <- 0
prob_csi <- lapply(csi, function(x) {1 - inla.pmarginal(a, x)})
## for each location estimate the probability in continous
cont_prob_cs1 <- data.frame(maps_cont_prob_csi=unlist(prob_csi)) %>% 
  tibble::rownames_to_column("ID_2") %>% 
  mutate(ID_2=gsub("index.","",ID_2))

maps_cont_prob_csi <- cont_prob_cs1
## for each location estimate the probability in groups
prob_csi_cutoff <- c(0,0.2,0.4,0.6,0.8,1) ## can change accordingly
cat_prob_csi <- cut(unlist(prob_csi),
                    breaks=prob_csi_cutoff, 
                    include.lowest=TRUE)


maps_cat_prob_csi <- data.frame(ID_2=unique(zim_child_model$id_2), ## check whether it joins well
                                cat_prob_csi=cat_prob_csi)
maps_cat_prob_csi$ID_2 <- as.character(maps_cat_prob_csi$ID_2)

zim_shp_df <- broom::tidy(zim_shp, region = "ID_2")
zim_shp_df <- zim_shp_df %>% 
  left_join(maps_cat_prob_csi, by=c("id"="ID_2")) %>% 
  left_join(maps_cont_prob_csi, by=c("id"="ID_2"))

glimpse(zim_shp_df)

## plot the results from INLA
p2 <- ggplot() + 
  geom_polygon(data = zim_shp_df, aes(x = long, y = lat,
                                      group = group,
                                      fill = maps_cont_prob_csi), 
               colour = "white") + theme_void() +
  ggtitle("RINLA Fit") + labs(fill = "P of  high HAZ") +
  scale_fill_continuous(high = "#fff7ec", low = "#7F0000")
p2
p1 <- p1 + scale_fill_continuous(high = "#fff7ec", low = "#7F0000") +
  ggtitle("Open Bugs Fit") + labs(fill = "Average HAZ")

## combine both inla and Bugs plots
gridExtra::grid.arrange(p1,p2) 


# zim_shp_ss <- zim_shp
# dat_zim <- attr(zim_shp_ss, "data")
# attr(zim_shp_ss, "data") <- merge(dat_zim, maps_cat_prob_csi, by="ID_2")
# spplot(obj=zim_shp_ss, zcol= "cat_prob_csi", col.regions=gray(seq(0.9,0.1,length=3)))

# HEADER --------------------------------------------
#
# Author: Diego Dinamarca
# Email:  ddinamarcamuller@gmail.com
# 
# Date:
#
# Script Name:
#
# Script Description: 
#
# Notes:
#
#
# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse",
              "terra", 
              "sf",
              "here") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new.pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

# install missing packages
if(length(new.pkg)){
  install.packages(new.pkg)
}

# load all requried libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
}
# SET WORKING DIRECTORY -----------------------------
cat("SETTING WORKING DIRECTORY...\n\n", sep = "")
wd <- here::here()
setwd(wd)
cat("WORKING DIRECTORY HAS BEEN SET TO: ", wd, sep = "")

# SET OPTIONS ---------------------------------------
cat("SETTING OPTIONS... \n\n", sep = "")
options(scipen = 999) # turns off scientific notation
options(encoding = "UTF-8") # sets string encoding to UTF-8 instead of ANSI


conflicted::conflict_prefer("select", winner = "dplyr")
conflicted::conflict_prefer("filter", winner = "dplyr") 
# LOAD FUNCTIONS ------------------------------------
stand_soilatt = function(obs.data, var, lambda, vlow = 0, vhigh = 1){
  require(aqp)
  require(ithir)
  #Define SoilProfile database
  depths(obs.data) = ID ~ top + bottom
  
  #smoothing parameter
  lambda.l <- c(10, 1, 0.1, 0.01, 0.001,0.0001,0.00001)
  
  #GET RMSE for each lambda
  rmse_models <- sapply(lambda.l, function(x){
    tryCatch({
      # x <- 10
      eaFit <- ea_spline(obs.data, var.name = var, #CAMBIAR COLUMNA SEGUN PROPIEDAD
                         d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = x, vlow = vlow, vhigh = vhigh,
                         show.progress = TRUE)
      print(paste0("lambda: ",x, " RMSE: ",round(eaFit$splineFitError$rmse %>% mean,3)))},
      error = function(w){
        print(paste0("Error fitting splines for lambda = ", x))
        res <- 9999
      })
  })
  # Get final results
  eaFit <- ea_spline(obs.data, var.name = var,
                     d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = lambda, vlow = vlow, vhigh = vhigh,
                     show.progress = F)
  return(eaFit)
}

# space reserved for your functions

# Load database
soilprofiles_data <- read_csv(here("data","soil_database","BD_soilprof_26ENE.csv"))
names(soilprofiles_data)

db = soilprofiles_data %>%
  filter(Tratamiento == "testigo") %>% 
  select(ID, x, y, top, bottom, bulkd = Da, clay = A, silt = L, sand = a, OM = MO, FC = CC, PWP = PMP)

# variables to process
variables <- c("bulkd","clay","sand","silt","OM", "FC","PWP")
# l <- c(0.1, 0.001,0.001, 0.001, 0.001)
l <- 0.0001
for (i in 1:7){
  #iterate through databases
  # i=1
  var <- variables[i]
  print(var)
  # clean non valid records
  db = db[which(db[[var]] != -999),]
  #Define SoilProfile database
  eaFit = stand_soilatt(db, var, l, vlow = 0, vhigh = 2.5)
  
  # Export Harmonized database
  harm <- eaFit$harmonised
  index <- match(harm$id, soilprofiles_data$ID)
  harm$x <- soilprofiles_data$x[index]
  harm$y <- soilprofiles_data$y[index]
  harm$Fuente <- soilprofiles_data$Fuente[index]
  
  # Look for profiles that have -9999 values
  w.id = harm %>% pivot_longer( cols = 2:7, names_to = "depth", values_to = var) %>% 
    filter(if_any(all_of(var), ~ .== -9999)) %>% 
    .[["id"]] %>% 
    unique
  
  # export observed data for wrong standardized soil profiles
  filter(soilprofiles_data, ID %in% w.id) %>% 
    write_csv(here("proc","soil_database",paste0("soilp_wrong_stand_",var,".csv")))
  
  harm[harm == -9999] <- NA
  
  #Write result
  fname = paste0(var,"_standard.csv")
  write_csv(harm,here("proc","soil_database","standard",fname))
}

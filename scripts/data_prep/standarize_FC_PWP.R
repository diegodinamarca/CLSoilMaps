# HEADER --------------------------------------------
#
# Author: Diego Dinamarca
# Email:  ddinamarcamuller@gmail.com
# 
# Date:
#
# Script Name: standarize_FC_PWP
#
# Script Description: standardize soil profiles for FC and PWP for comparing with soil maps
#
# Notes:
#
#
# INSTALL PACKAGES & LOAD LIBRARIES -----------------
cat("INSTALLING PACKAGES & LOADING LIBRARIES... \n\n", sep = "")
packages <- c("tidyverse",
              "terra", 
              "sf",
              "here",
              "ithir",
              "aqp",
              "RColorBrewer") # list of packages to load
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

# LOAD FUNCTIONS ------------------------------------
source(here("scripts","analysis","standarize_soilprof.R"))
# space reserved for your functions

# Load soil database ------------------------------------------------------
soilprofiles_data = read_csv(here("data","soil_database","Soilprofile_db.csv"))
data = soilprofiles_data %>%
  filter(Tratamiento == "testigo") %>% 
  select(ID, x, y, top, bottom, FC = CC, PWP = PMP)

fc.data = filter(data, FC != -999) %>% select(-PWP)
pwp.data = filter(data, PWP != -999) %>% select(-FC)

# check registers that have missing PWP or FC
filter(data, FC != -999 & PWP == -999) %>% 
  select(ID, top, bottom) %>% 
  left_join(soilprofiles_data) %>%
  write_csv(here("proc","soil_database","missing_pwp.csv"))
filter(data, FC == -999 & PWP != -999) %>% 
  select(ID, top, bottom) %>% 
  left_join(soilprofiles_data) %>%
  write_csv(here("proc","soil_database","missing_fc.csv"))


# Standardize FC ----------------------------------------------------------
eaFit = stand_soilatt(fc.data, "FC", 0.1)

# Export Harmonized database
harm <- eaFit$harmonised
index <- match(harm$id, soilprofiles_data$ID)
harm$x <- soilprofiles_data$x[index]
harm$y <- soilprofiles_data$y[index]
harm$Fuente <- soilprofiles_data$Fuente[index]

# Look for profiles that have -9999 values
w.id = harm %>% pivot_longer( cols = 2:7, names_to = "depth", values_to = "FC") %>% 
  filter(FC == -9999) %>% 
  .[["id"]] %>% 
  unique

# export observed data for wrong standardized soil profiles
filter(soilprofiles_data, ID %in% w.id) %>% 
  write_csv(here("proc","soil_database","soilp_wrong_stand_FC.csv"))

# remove problematic values
harm[harm == -9999] <- NA

# Write result
write_csv(harm, here("proc","soil_database","FC_standard.csv"))


# Standardize PWP ---------------------------------------------------------
eaFit = stand_soilatt(pwp.data, "PWP", 0.1)
# Export Harmonized database
harm <- eaFit$harmonised
index <- match(harm$id, soilprofiles_data$ID)
harm$x <- soilprofiles_data$x[index]
harm$y <- soilprofiles_data$y[index]
harm$Source <- soilprofiles_data$Fuente[index]

# Look for profiles that have -9999 values
w.id = harm %>% pivot_longer( cols = 2:7, names_to = "depth", values_to = "PWP") %>% 
  filter(PWP == -9999) %>% 
  .[["id"]] %>% 
  unique

# export observed data for wrong standardized soil profiles
filter(soilprofiles_data, ID %in% w.id) %>% 
  write_csv(here("proc","soil_database","soilp_wrong_stand_PWP.csv"))

# remove problematic values
harm[harm == -9999] <- NA
# Write result
write_csv(harm, here("proc","soil_database","PWP_standard.csv"))




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
              "here",
              "scico") # list of packages to load
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
# space reserved for your functions



# Load Dataframe with observed and simulated values -----------------------

# Field Capacity
fname = here("proc","correlations","FC_correlation.csv")
data = read_csv(fname) %>% 
  mutate(depth = factor(depth, levels = depths))

# depths levels
depths <- c('0-5 cm','5-15 cm','15-30 cm', "30-60 cm","60-100 cm","100-200 cm")

# calculate metrics
annotate_metrics = function(sim, obs) {
  m = hydroGOF::gof(sim, obs, norm = "maxmin")
  msg = paste0("R =", m["r",], "\nRMSE =", m["RMSE",], "\nPBIAS =", m["PBIAS %",],"%")
  as.character(msg)
}
metrics = data %>% group_by(depth) %>% do(data.frame(message = annotate_metrics(.$sim, .$obs)))

# set default theme
theme_set(theme_bw())
p = ggplot(data, aes(x = obs, y = sim))+
  geom_point(alpha = 0.7)+
  geom_label(data = metrics, aes(label = message), x = 0.2, y = 0.7, size = 2)+
  geom_smooth(method = "lm", se = T)+
  geom_abline(slope = 1, intercept = 0)+
  tune::coord_obs_pred()+
  labs(x = "observed", y = "simulated", title = "Field Capacity")+
  facet_wrap(depth~.)
ggsave(filename = here("results","figures","FC_correlation.png"), width = 8, height = 6, plot = p)


# Permanent Wilting Point
fname = here("proc","correlations","PWP_correlation.csv")
data = read_csv(fname) %>% 
  mutate(depth = factor(depth, levels = depths))

# depths levels
depths <- c('0-5 cm','5-15 cm','15-30 cm', "30-60 cm","60-100 cm","100-200 cm")

# calculate metrics
annotate_metrics = function(sim, obs) {
  m = hydroGOF::gof(sim, obs, norm = "maxmin")
  msg = paste0("R =", m["r",], "\nRMSE =", m["RMSE",], "\nPBIAS =", m["PBIAS %",],"%")
  as.character(msg)
}
metrics = data %>% group_by(depth) %>% do(data.frame(message = annotate_metrics(.$sim, .$obs)))

# set default theme
theme_set(theme_bw())
p = ggplot(data, aes(x = obs, y = sim))+
  geom_point(alpha = 0.7)+
  geom_label(data = metrics, aes(label = message), x = 0.2, y = 0.7, size = 2)+
  geom_smooth(method = "lm", se = T)+
  geom_abline(slope = 1, intercept = 0)+
  tune::coord_obs_pred()+
  labs(x = "observed", y = "simulated", title = "Permanent Wilting Point")+
  facet_wrap(depth~.)
ggsave(filename = here("results","figures","PWP_correlation.png"), width = 8, height = 6, plot = p)

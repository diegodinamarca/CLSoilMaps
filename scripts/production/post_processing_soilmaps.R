####POST-PROCESSING OF SOIL MAPS######
library(terra)
library(tidyverse)

# output directory
outdir <- "D:/CLSoilMaps/results/postproc/v1/"
dir.create(outdir)

# input files
files <- list.files("D:/CLSoilMaps/proc/soilmaps/", pattern = ".tif", full.names = T);head(files)
imgs <- rast(files)

# water bodies
water.bodies <- vect("D:/CLSoilMaps/data/shp/masas_de_agua/masas_lacustres.shp")
water.bodies <- project(water.bodies, imgs)
masked <- mask(imgs, water.bodies, inverse = T)

# water bodies in argentina for binational basins
water.bodies.2 <- vect("D:/CLSoilMaps/data/shp/masas de agua/areas_de_aguas_continentales_perenne.kml")
masked <- mask(masked, water.bodies.2, inverse = T)

# urban areas
urban <- vect("D:/CLSoilMaps/data/shp/Areas_Pobladas/Areas_Pobladas.shp");urban
masked <- mask(masked, urban, inverse = T)

# round to 3 decimals
rounded <- round(masked, digits = 3)
names(rounded)

# spanish variable names
variables <- c("Arcilla.","Arena.","Da.","Limo.")

# standardized depths
depths <- c("5-15cm","100-200cm","0-5cm","15-30cm","30-60cm","60-100cm")

# set filenames
f.names<-lapply(variables, function(v){paste0(v,depths)}) %>% unlist;f.names
names(rounded) <- f.names

# export results
writeRaster(rounded, paste0(outdir, f.names, ".tif"), overwrite = T)




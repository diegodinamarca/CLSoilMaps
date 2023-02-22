####POST-PROCESSING OF SOIL TEXTURAL CLASS MAPS######
library(terra)
library(tidyverse)

# output directory
outdir <- "D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3/clases texturales/"
dir.create(outdir)

# input files
files <- list.files("D:/paper_BDsuelos/Versiones/Version3/clases_texturales", pattern = ".tif$", full.names = T);head(files)
imgs <- rast(files)

# water bodies
water.bodies <- vect("./materiales/masas de agua/masas_lacustres.shp")
masked <- mask(imgs, water.bodies, inverse = T)

# water bodies in argentina for binational basins
water.bodies.2 <- vect("./materiales/masas de agua/areas_de_aguas_continentales_perenne.kml")
masked <- mask(masked, water.bodies.2, inverse = T)

# urban areas
urban <- vect("./materiales/Areas_Pobladas/Areas_Pobladas.shp");urban
masked <- mask(masked, urban, inverse = T)

# variable name
variable <- c("Tex_Class.")

# standardized depths
depths <- c("0-5cm","100-200cm","15-30cm","30-60cm","5-15cm","60-100cm")

# filenames
f.names<-paste0(variable, depths);f.names
names(masked) <- f.names

# Export results
writeRaster(masked, paste0(outdir, f.names, ".tif"), overwrite = T)
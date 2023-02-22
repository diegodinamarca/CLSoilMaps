#### POST PROCESADO DE MAPAS DE CLASES TEXTURALES######

library(terra)
library(tidyverse)

# Carpeta de salida
outdir <- "D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3/clases texturales/"
dir.create(outdir)

# archivos de entrada
files <- list.files("D:/paper_BDsuelos/Versiones/Version3/clases_texturales", pattern = ".tif$", full.names = T);head(files)
imgs <- rast(files)

# Cuerpos de Agua
water.bodies <- vect("./materiales/masas de agua/masas_lacustres.shp")
# water.bodies <- project(water.bodies, imgs)
masked <- mask(imgs, water.bodies, inverse = T)

# Areas Urbanas
urban <- vect("./materiales/Areas_Pobladas/Areas_Pobladas.shp");urban
# urban <- project(urban, masked)
masked <- mask(masked, urban, inverse = T)

# nombre de variables
variable <- c("Tex_Class.")
# profundidades
depths <- c("0-5cm","100-200cm","15-30cm","30-60cm","5-15cm","60-100cm")
#vector con nombre de las capas
f.names<-paste0(variable, depths);f.names
names(masked) <- f.names
# Exportar resultados
writeRaster(masked, paste0(outdir, f.names, ".tif"), overwrite = T)

list.files("D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3", pattern = "Tex_", full.names = TRUE) %>% rast

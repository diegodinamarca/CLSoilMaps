#### POST PROCESADO DE MAPAS DE SUELO######

library(terra)
library(tidyverse)


# Carpeta de salida
outdir <- "D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3"
dir.create(outdir)

# archivos de entrada
files <- list.files("D:/paper_BDsuelos/Versiones/Version3/", pattern = ".tif", full.names = T);head(files)
imgs <- rast(files)

# Cuerpos de Agua
water.bodies <- vect("./materiales/masas de agua/masas_lacustres.shp")
water.bodies <- project(water.bodies, imgs)
masked <- mask(imgs, water.bodies, inverse = T)

# Cuerpos de Aguas Argentina
water.bodies.2 <- vect("./materiales/masas de agua/areas_de_aguas_continentales_perenne.kml")
masked <- mask(masked, water.bodies.2, inverse = T)

# Areas Urbanas
urban <- vect("./materiales/Areas_Pobladas/Areas_Pobladas.shp");urban
# urban <- project(urban, masked)
masked <- mask(masked, urban, inverse = T)

# Redondear a 3 decimales
rounded <- round(masked, digits = 3)
names(rounded)

# nombre de variables
variables <- c("Arcilla.","Arena.","Da.","Limo.")
# profundidades
depths <- c("5-15cm","100-200cm","0-5cm","15-30cm","30-60cm","60-100cm")
#vector con nombre de las capas
f.names<-lapply(variables, function(v){paste0(v,depths)}) %>% unlist;f.names
names(rounded) <- f.names
# Exportar resultados
writeRaster(rounded, paste0(outdir, f.names, ".tif"), overwrite = T)




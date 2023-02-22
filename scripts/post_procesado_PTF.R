#### POST PROCESADO DE MAPAS DE SUELO######

library(terra)
library(tidyverse)

# Carpeta de salida
outdir <- "D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3/PTF_LowerLim"
dir.create(outdir)

# archivos de entrada
files <- list.files("D:/PTFs/PTF_results_LowerLim", pattern = ".tif", full.names = T);head(files)
atts <- c("CC","PMP","HA","AWC","ksat","alpha","n","theta_r","theta_s")
for (i in 1:1) {
  print(i)
  for (j in 1:6) {
    cat(j," ")
    
    att <- atts[i]
    index <- grep(atts[i], files)
    imgs <- rast(files[index][j])
    # imgs <- imgs[[j]]
    # # Cuerpos de Agua
    # water.bodies <- vect("D:/paper_BDsuelos/materiales/masas de agua/masas_lacustres.shp")
    # # water.bodies <- project(water.bodies, imgs)
    # masked <- mask(imgs, water.bodies, inverse = T)
    # plot(masked)
    # # Areas Urbanas
    # urban <- vect("D:/paper_BDsuelos/materiales/Areas_Pobladas/Areas_Pobladas.shp");urban
    # # urban <- project(urban, masked)
    # masked <- mask(masked, urban, inverse = T)
    r <- rast("D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3/Da.100-200cm.tif")
    imgs <- extend(imgs, r)
    imgs;r
    masked <- mask(imgs, r)
    # Redondear a 3 decimales
    rounded <- round(masked, digits = 3)
    
    # profundidades
    depths <- c("5-15cm","100-200cm","0-5cm","15-30cm","30-60cm","60-100cm")
    lyr.nam <- paste0(att,"_", depths)
    names(rounded) <- lyr.nam[j]
    names(rounded)
    # Exportar resultados
    writeRaster(rounded, paste0(outdir,"/", lyr.nam[j], ".tif"), overwrite = T)
  }
  

}
imgs <- rast(files[55]);imgs
r <- rast("D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3/Da.100-200cm.tif")
imgs <- extend(imgs, r)
masked <- mask(imgs, r)

# water.bodies <- vect("D:/paper_BDsuelos/materiales/masas de agua/areas_de_aguas_continentales_perenne.kml")
# masked <- mask(masked, water.bodies, inverse = T)

rounded <- round(masked, digits = 3)
names(rounded) <- "Total_AWC"
# Exportar resultados
writeRaster(rounded, paste0(outdir,"/", "Total_AWC_0_200cm", ".tif"), overwrite = T)
beepr::beep(sound = 4)





#Load libraries
library(terra)
library(sf)
library(sp)
library(raster)
library(tidyverse)


# LOAD COVARIATES ------------------------------------------------------
covs <- c()

#topography
f <- list.files(path = "./materiales/topografia/derivadas_topo",pattern = "tif$",full.names = T);f
covs <- c(covs,f)
der_topo <- rast(f)

#Climate
f <- list.files(path = "./materiales/Climate", pattern = ".tif$",full.names = T);f
covs <- c(covs,f)
climate <- rast(f)

#Spectral indexes
f <- list.files(path = "./materiales/Landsat/indices",pattern = ".tif",full.names = T);f
covs <- c(covs,f)
indices <- rast(f)

#Textures 11x11
f <- list.files(path = "./materiales/Landsat/indices/GLCM11x11",full.names = T);f
covs <- c(covs,f)
texturas11x11 <- rast(f)

#Textures 21x21
f <- list.files(path = "./materiales/Landsat/indices/GLCM21x21",full.names = T);f
covs <- c(covs,f)
texturas21x21 <- rast(f)

#Textures 51x51
f <- list.files(path = "./materiales/Landsat/indices/GLCM51x51",full.names = T);f
covs <- c(covs,f)
texturas51x51 <- rast(f)

#Geology
f <- list.files(path = "./materiales/Geologia/rast", pattern = ".tif$", full.names = T);f
covs <- c(covs,f)
geo <- rast(f)

saveRDS(object = covs, file="./covariables.rds")

# Extract Values from standardized soil profiles ---------------------------------------------------------

# soil profiles directory
indir <- "./materiales/Database/estandarizadas"

# soil phyisical properties
variables <- c("Arcilla","arena","Da","Limo","MO")

for (i in 4:4){
  var <- variables[i]
  cat("Extrayendo valores para", var, "\n")
  
  # output directory
  out <- paste0("./materiales/Database/",var,"/");out
  dir.create(path = out, showWarnings = F)
  
  # read tifs
  files <- list.files(path = indir, pattern = "estandarizada", full.names = T);print(files)
  
  bd <- files[grep(paste0(var,"_"), files)] %>% read_csv()
  coordinates(bd) <- c("x","y")
  crs(bd) <- "+proj=longlat +datum=WGS84 +no_defs"
  bd <- vect(bd)
  
  #topography
  der_topo.val <- terra::extract(der_topo, bd)
  
  #climate
  climate.val <- terra::extract(climate, bd )
  
  #textural metrics 11x11 LANDSAT
  tex11x11.val <- terra::extract(texturas11x11, bd )
  
  #textural metrics 21x21 LANDSAT
  tex21x21.val <- terra::extract(texturas21x21, bd )
  
  #textural metrics 51x51 LANDSAT
  tex51x51.val <- terra::extract(texturas51x51, bd )
  
  #Spectral index
  indices.val <- terra::extract(indices, bd )
  
  #Geology
  geo.val <- terra::extract(geo, bd )
  
  #create and export spatial DB
  ext.val <- bind_cols(der_topo.val, climate.val, tex51x51.val, tex21x21.val, tex11x11.val, indices.val, geo.val)
  ext.val <- ext.val %>% dplyr::select(-starts_with("ID"))
  
  data <- bind_cols(as.data.frame(bd), ext.val)
  data <- data %>% tibble
  if (var == "MO"){
    data$`30-60 cm` <- data$`30-60 cm` %>% as.numeric
    data$`60-100 cm`<- data$`60-100 cm` %>% as.numeric
    data$`100-200 cm`<- data$`100-200 cm` %>% as.numeric
  }
  
  data %>%  pivot_longer(cols = 2:7, names_to = "depth", values_to = var) %>%
    dplyr::mutate(depth = rep(c(2.5, 10, 22.5, 45, 80, 150), nrow(bd)),
           id = as.numeric(id)) %>% 
    arrange(id) %>% 
    dplyr::select(id, Fuente, var, depth, prof_max = 'soil depth', everything()) %>% 
    write_csv(paste0(out,var,".csv"))
}

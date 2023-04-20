library(terra)
library(sf)
library(tidyverse)
library(here)

conflicted::conflict_prefer("extract", "terra")
conflicted::conflict_prefer("filter", "dplyr")

# ecoregiones
eco = read_sf("D:/CLSoilMaps/data/shp/ecoregiones/ecoregions_chile_camels_dissolved.shp") %>%
  st_transform(4326) %>%
  filter(ECO_NAME != "Rock and Ice")

# variables a extraer
variables = c("Bulkd", "Clay","Sand","Silt")

# mapas
files = list.files(here("results","postproc","v1","SoilMaps_MEAN"),
                   pattern = ".tif", full.names = TRUE);files
imgs = rast(files);names(imgs)

# SoilGrids Maps
sg.files = list.files("D:/CLSoilMaps_old/materiales/SoilGrids/SG_maps",
                   full.names = TRUE);sg.files



sf_use_s2(FALSE)
dataset = lapply(1:4, function(i){
  # i=2
  index = grep(pattern = variables[i], files)
  index
  imgs_i = imgs[[index]];names(imgs_i)
  
  # standardized soilprofiles
  files = list.files("D:/CLSoilMaps_old/materiales/Database/estandarizadas", full.names = TRUE);files
  var_esp = c("Da","Arcilla","arena","Limo")
  index = grep(paste0("^",var_esp[i]), basename(files));index
  
  
  # Esta parte de aqui es para leer las base de datos de la ultima version
  # files = list.files(here("D:/CLSoilMaps/proc/soil_database/standard"), full.names = TRUE);files
  # index = grep(tolower(variables[i]), files);index
  soildata = read_csv(files[index]) %>%
    select(-c(`soil depth`, Fuente)) %>% 
    pivot_longer(cols = 2:7) %>% 
    filter(!is.na(value))
  soil_points = st_as_sf(soildata, coords = c("x","y"), crs = 4326)
  
  # inter = st_intersection(sp.utm, eco.utm %>% select(ECO_NAME))
  inter.file = here("proc","vector",
                     paste0("soilpoints_inter_ecoregions_",variables[i],".geojson"))
  inter.file
  if (!file.exists(inter.file)){
    inter = st_intersection(soil_points, eco %>% select(ECO_NAME))
    write_sf(inter, inter.file)
  }else{
    inter=read_sf(inter.file)
  }
  soil_points %>% filter(!(id %in% inter$id)) %>% 
    write_sf(here("proc","vector",
                  paste0("soilpoints_notin_ecoregions_",variables[i],".geojson")))
  r = imgs_i[[c(1,5,3,4,6,2)]]
  extr = lapply(1:6, function(k){
    depths = inter$name %>% unique
    points = inter %>% filter(name == depths[k])
    extr = terra::extract(r[[k]], points)
    tibble(var = variables[i],
           eco_name = points$ECO_NAME,
           depth = depths[k], 
           value_sim = extr[[2]], 
           value_obs = points$value)
  }) %>% bind_rows()
  
  index = grep(tolower(variables[i]), sg.files);index
  r.sg = rast(list.files(sg.files[index], full.names = TRUE))
  inter = st_transform(inter, crs(r.sg))
  extr2 = lapply(1:6, function(k){
    depths = inter$name %>% unique
    points = inter %>% filter(name == depths[k])
    extr = terra::extract(r.sg[[k]], points)
    if (variables[i] == "Bulkd"){
      tibble(var = variables[i],
             eco_name = points$ECO_NAME,
             depth = depths[k], 
             value_sg = extr[[2]]/100, 
             value_obs = points$value)
    }else{
      tibble(var = variables[i],
             eco_name = points$ECO_NAME,
             depth = depths[k], 
             value_sg = extr[[2]]/10, 
             value_obs = points$value)
    }
    
  }) %>% bind_rows()
  extr2
  full_join(extr, extr2, by = c("var","eco_name","depth","value_obs"))
}) %>% bind_rows()


write_csv(dataset, file = here("proc","tables","extr_stand_soilprof_paper.csv"))


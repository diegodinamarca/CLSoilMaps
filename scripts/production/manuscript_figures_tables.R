{
  # HEADER --------------------------------------------
  #
  # Author: Diego Dinamarca
  # Email:  ddinamarcamuller@gmail.com
  # 
  # Date:
  #
  # Script Name:
  #
  # Script Description: figures and tables for the manuscript
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
                "gridExtra",
                "raster",
                "rasterVis",
                "ggpointdensity",
                "viridis",
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
  
  
  # CONFLICTS ---------------------------------------------------------------
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("extract", "terra")
  
  # LOAD FUNCTIONS ------------------------------------
  # Extraer predicciones
  get_pred_stats <- function(var, model, predictions){
    pred.list <- list(rep(NA, 6))
    for (i in 1:6){
      # i = 1
      pred.depth <- predictions %>% filter(depth == depths[i])
      
      #calculate mean prediction and residuals
      n <- pred.depth %>% nrow()/100;n
      # model = "QRF"
      if (model == "QRF"){
        qpred.list <- lapply(1:21, function(j){
          qpred <- pred.depth[[j]] %>% matrix(nrow = n, byrow = F) %>%
            as.data.frame() %>%
            rowMeans()
        }) %>% rlist::list.cbind() %>% as_tibble
        names(qpred.list) <- names(pred.depth)[1:21]
        qpred.list <- qpred.list %>%
          bind_cols(depth = depths[i], obs = pred.depth$obs[1:n])%>%
          mutate(error = obs-q0.5);qpred.list
        pred.list[[i]] <- qpred.list  
      }
      if (model == "RF"){
        pred.means <- pred.depth$predictions %>% matrix(nrow = n, byrow = F) %>%
          as.data.frame() %>%
          rowMeans() %>% 
          bind_cols(pred.depth$obs[1:n]) %>%
          rename(pred.mean = 1, obs = 2) %>%
          mutate(error = obs-pred.mean,
                 depth = depths[i])
        
        pred.var <- pred.depth$predictions %>% matrix(nrow = n, byrow = F) %>%
          matrixStats::rowVars() %>%
          as.data.frame() %>%
          rename(pred.var = 1)
        pred.var %>% bind_cols(pred.depth$obs[1:n])
        
        pred.list[[i]] <- bind_cols(pred.means, pred.var) %>% dplyr::select(c(2, 1, 5, 3, 4))
      }
      
    }
    pred.stats <- rlist::list.rbind(pred.list);pred.stats
    pred.stats
    
    return(pred.stats)
  }
  # space reserved for your functions
}
###################### PARTE 1: FILTRAR DATOS DE 5-15CM PARA CARTOGRAFIAS ####################################
dir = here("proc","soil_database","standard")
files <- list.files(dir, pattern = "standard", full.names = T);files
arcilla <- read_csv(files[grep("clay", files)])
arena <- read_csv(files[grep("sand", files)])
da <- read_csv(files[grep("bulkd", files)])
limo <- read_csv(files[grep("silt", files)])

# Filtrar datos de 5-15cm
arcilla <- arcilla %>% pivot_longer( cols = 2:7, names_to = "profundidad", values_to = "values") %>% 
  filter(profundidad == "0-5 cm") %>% drop_na()

arena <- arena %>% pivot_longer( cols = 2:7, names_to = "profundidad", values_to = "values") %>% 
  filter(profundidad == "0-5 cm") %>% drop_na()

da <- da %>% pivot_longer( cols = 2:7, names_to = "profundidad", values_to = "values") %>% 
  filter(profundidad == "0-5 cm") %>% drop_na()
  
limo <- limo %>% pivot_longer( cols = 2:7, names_to = "profundidad", values_to = "values") %>% 
  filter(profundidad == "0-5 cm") %>% drop_na()

here("proc","soil_database","H1.0_5cm","clay_0_5cm.csv")
# exportar
write_csv(arcilla, here("proc","soil_database","H1.0_5cm","clay_0_5cm.csv"))
write_csv(arena, here("proc","soil_database","H1.0_5cm","sand_0_5cm.csv"))
write_csv(da, here("proc","soil_database","H1.0_5cm","bulkd_0_5cm.csv"))
write_csv(limo, here("proc","soil_database","H1.0_5cm","silt_0_5cm.csv"))

####################### FIN PARTE 1 ######################
#####-----------------------------------------------######
####################### PARTE 2 (NOT UPDATED): PUNTOS POR UNIDAD GEOMORFOLOGICA ###################################
# area de estudio para la proyeccion
ae <- rast("./materiales/area_estudio/ae_buffer_0.001deg.tif")

# leer BD estandarizadas
files <- list.files("./materiales/Database/estandarizadas", pattern = "estandarizada", full.names = T);files
tex <- read_csv(files[1])
da <- read_csv(files[3])
tex <- tex %>% pivot_longer(cols = 2:7, names_to = "depth", values_to = "values") %>% drop_na()
da <- da %>% pivot_longer(cols = 2:7, names_to = "depth", values_to = "values") %>% drop_na()
tex;da

# Crear BD espacial texturas
tex <- st_as_sf(tex,  coords = c("x","y"),crs = 4326)
tex <- dplyr::select(tex, c(depth, values))

# Fix depth names
depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")
fct.depths <- factor(c("0-5","5-15","15-30","30-60","60-100","100-200"), levels = c("0-5","5-15","15-30","30-60","60-100","100-200"))
tex$depth <- tex$depth %>% str_sub(start = 1, end = nchar(.)-3) 
tex$depth <- factor(tex$depth, levels = fct.depths)

# Crear BD espacial densidad aparente
da <- st_as_sf(da, coords = c("x","y"), crs = 4326)
da <- dplyr::select(da, c(depth, values))

# Fix depth names
da$depth <- da$depth %>% str_sub(start = 1, end = nchar(.)-3) 
da$depth <- factor(da$depth, levels = fct.depths)

# Cargar unidades geomorfologicas
ugeo <- st_read("./materiales/unidades_geomorfologicas/unidades_geomorfologicas.shp", crs = 4326)
ugeo.csv <- read_csv("./materiales/unidades_geomorfologicas/ugeo.csv")
ugeo$ID = seq(1:nrow(ugeo)) #agregar columna ID

##################### CONFIGURACIONES
sf::sf_use_s2(FALSE) # Remove spherical geometries error
theme_set(theme_bw())
depths <- c("0-5 cm","5-15 cm","15-30 cm","30-60 cm","60-100 cm","100-200 cm")
colpal <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03")
colScale <- scale_fill_manual(values = colpal, limits = levels(fct.depths), labels = depths, drop = F)

# Plot
# Mapa con los puntos en cada ugeo
# colpal <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03")
# ggplot() +
#   geom_sf(data = ugeo, aes(fill = r_name_eng), color = NA) +
#   geom_sf(data = tex, size = 1, color = "black") +
#   # geom_sf(data = bd, size = 1, color = "yellow")+
#   scale_fill_manual(values = colpal)

inter <- st_intersection(ugeo, da)
ndatos <- inter %>% group_by(r_name_eng, depth) %>% 
  summarise(n_datos = n()) %>% tibble %>% 
  # mutate(depth = factor(depth)) %>% 
  mutate(depth = factor(depth, levels = rev(levels(fct.depths))))

ggplot(ndatos) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "Bulk Density", y = "Number of Points", x = "Geomorphologic Unit", fill = "Depth")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30))+
  guides()
  ggsave(filename = "./plots/puntos_por_ugeo_bulkd.png")


inter <- st_intersection(ugeo, tex)
ndatos <- inter %>% group_by(r_name_eng, depth) %>% 
  summarise(n_datos = n()) %>% tibble %>% 
  # mutate(depth = factor(depth)) %>% 
  mutate(depth = factor(depth, levels = rev(levels(fct.depths))))
ggplot(ndatos) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "Textures", y = "Number of Points", x = "Geomorphologic Unit", fill = "Depth")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30))
ggsave(filename = "./plots/puntos_por_ugeo_textures.png")

####################### FIN PARTE 2 ######################
#####-----------------------------------------------######
############# PARTE 3 (NOT UPDATED): PUNTOS POR UNIDAD GEOMORFOLOGICA Y ZONA CLIMATICA ###############################################################

##################### CONFIGURACIONES
sf::sf_use_s2(FALSE) # Remove spherical geometries error
theme_set(theme_bw())
depths <- c("0-5 cm","5-15 cm","15-30 cm","30-60 cm","60-100 cm","100-200 cm")
fct.depths <- factor(c("0-5","5-15","15-30","30-60","60-100","100-200"), levels = c("0-5","5-15","15-30","30-60","60-100","100-200"))
colpal <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03")
colScale <- scale_fill_manual(values = colpal, limits = levels(fct.depths), labels = depths, drop = F)

# area de estudio para la proyeccion
ae <- rast("./materiales/area_estudio/ae_buffer_0.001deg.tif")
# leer BD estandarizadas
files <- list.files("./materiales/Database/estandarizadas", pattern = "estandarizada", full.names = T);files
tex <- read_csv(files[1])
da <- read_csv(files[3])
tex <- tex %>% pivot_longer(cols = 2:7, names_to = "depth", values_to = "values") %>% drop_na()
da <- da %>% pivot_longer(cols = 2:7, names_to = "depth", values_to = "values") %>% drop_na()
tex;da

# Crear BD espacial texturas
tex <- st_as_sf(tex,  coords = c("x","y"),crs = 4326)
tex <- dplyr::select(tex, c(depth, values))

# Fix depth names
depths <- c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm")
fct.depths <- factor(c("0-5","5-15","15-30","30-60","60-100","100-200"), levels = c("0-5","5-15","15-30","30-60","60-100","100-200"))
tex$depth <- tex$depth %>% str_sub(start = 1, end = nchar(.)-3) 
tex$depth <- factor(tex$depth, levels = fct.depths)

# Crear BD espacial densidad aparente
da <- st_as_sf(da, coords = c("x","y"), crs = 4326)
da <- dplyr::select(da, c(depth, values))

# Fix depth names
da$depth <- da$depth %>% str_sub(start = 1, end = nchar(.)-3) 
da$depth <- factor(da$depth, levels = fct.depths)

# Cargar unidades geomorfologicas
ugeo <- st_read("./materiales/unidades_geomorfologicas/unidades_geomorfologicas.shp", crs = 4326)
ugeo.csv <- read_csv("./materiales/unidades_geomorfologicas/ugeo.csv")
ugeo$ID = seq(1:nrow(ugeo)) #agregar columna ID


# Extent de cada zona climática
hyperarid <- ext(c(-78, -65, -32, -17))
mediterranean <- ext(c(-78, -65, -43, -32))
patagonia <- ext(c(-78, -65, -56, -43))
sf.arid <- st_as_sfc(hyperarid %>% st_bbox)
st_crs(sf.arid) <- st_crs(ugeo)
sf.medi <- st_as_sfc(mediterranean %>% st_bbox)
st_crs(sf.medi) <- st_crs(ugeo)
sf.pata <- st_as_sfc(patagonia %>% st_bbox)
st_crs(sf.pata) <- st_crs(ugeo)

# lista de zonas
zones <- list(sf.arid, sf.medi, sf.pata)
# cortar ugeo por zona climatica
crop.list <- lapply(zones, function(z){st_crop(ugeo, z)})

######### Interseccion densidad aparente y ugeo
inter.list <- lapply(crop.list, function(c){st_intersection(c, da)})
ndatos.list <- lapply(inter.list, function(i){
  i %>% group_by(r_name_eng, depth) %>% 
    summarise(n_datos = n()) %>% tibble %>% 
    # mutate(depth = factor(depth)) %>% 
    mutate(depth = factor(depth, levels = rev(levels(fct.depths))))
})

# Lista vacia
plots.list <- list()

temp.plot1 <- ggplot(ndatos.list[[1]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "Bulk Density", y = "", x = "Hyper/Semi Arid Zone")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none",
        axis.title.y = element_text(face = "bold")) 
# temp.plot1
temp.plot2 <- ggplot(ndatos.list[[2]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "", y = "", x = "Mediterranean Zone")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none",
        axis.title.y = element_text(face = "bold"))
# temp.plot2
temp.plot3 <- ggplot(ndatos.list[[3]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "", y = "Number of Points", x = "Rainy and Patagonia Zone")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none",
        axis.title.y = element_text(face = "bold"))
# temp.plot3

plots.list[[1]] <- temp.plot1
plots.list[[2]] <- temp.plot2
plots.list[[3]] <- temp.plot3

########### Interseccion texturas y ugeo
inter.list <- lapply(crop.list, function(c){st_intersection(c, tex)})
ndatos.list <- lapply(inter.list, function(i){
  i %>% group_by(r_name_eng, depth) %>% 
    summarise(n_datos = n()) %>% tibble %>% 
    # mutate(depth = factor(depth)) %>% 
    mutate(depth = factor(depth, levels = rev(levels(fct.depths))))
})

temp.plot1 <- ggplot(ndatos.list[[1]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "Textures", y = "", x = "")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none")
# temp.plot1
temp.plot2 <- ggplot(ndatos.list[[2]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "", y = "", x = "")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none")
# temp.plot2
temp.plot3 <- ggplot(ndatos.list[[3]]) +
  geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
  # theme(legend.position = "none")+
  colScale+
  labs(title = "", y = "Number of Points", x = "")+
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(angle = 30),
        legend.position = "none")
# temp.plot3
plots.list[[4]] <- temp.plot1
plots.list[[5]] <- temp.plot2
plots.list[[6]] <- temp.plot3


grob <- ggplotGrob(ggplot(ndatos) +
                     geom_col(mapping = aes(x = r_name_eng, y = n_datos, fill = depth), position = "dodge") +
                     colScale +
                     guides(fill = guide_legend(title = "Depth")))
id.legend <- grep("guide", grob$layout$name)
legend.depths <- grob[["grobs"]][[id.legend]]
plot(legend.depths)

plots.list[[7]] <- legend.depths
lay <- rbind(c(1,4,NA),
             c(2,5,7),
             c(3,6,NA))
grid.arrange(grobs = plots.list, layout_matrix = lay, legend, widths= c(3,3,1))
# cowplot::plot_grid(plotlist = plots.list[1:6], nrow = 3, ncol = 2)
# plot_grid(p1, p2, labels = c('A', 'B'), label_size = 12)

######################## FIN PARTE 3 #####################
#####-----------------------------------------------######
############# PARTE 4 (NOT UPDATED): DISTRIBUCION DE VALORES PREDICHOS POR UGEO Y ZONA CLIMATICA ################################################################

# area de estudio para la projeccion
ae <- rast("./materiales/area_estudio/ae_buffer_0.001deg.tif")

# Cargar unidades geomorfologicas
ugeo <- st_read("./materiales/unidades_geomorfologicas/unidades_geomorfologicas.shp", crs = 4326)
ugeo.csv <- read_csv("./materiales/unidades_geomorfologicas/ugeo.csv")
ugeo$ID = seq(1:nrow(ugeo)) #agregar columna ID
ugeo <- as(ugeo, "SpatVector")

# Cargar mapas de propiedades de suelos
files <- list.files("D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3", pattern = ".tif$", full.names = T)
files
# soil.maps <- files %>% rast
# names(soil.maps)

sand.maps <- grep("Arena", files)
clay.maps <- grep("Arcilla", files)
bulkd.maps <- grep("Da", files)
silt.maps <- grep("Limo", files)


soil.maps <- rast(files[c(clay.maps, sand.maps, bulkd.maps, silt.maps)])
names(soil.maps)
# soil.maps <- project(soil.maps, "epsg:4326")
variables <- c("Clay.","Sand.","BulkD.", "Silt.")
depths <- c("0-5cm","100-200cm","15-30cm","30-60cm","5-15cm","60-100cm")
f.names<-lapply(variables, function(v){paste0(v,depths)}) %>% unlist;f.names
names(soil.maps) <- f.names
soil.names <- names(soil.maps);soil.names


# vector de variables 
variables <- c("BulkD","Clay","Sand", "Silt")
var <- variables[1]
var.names <- c("BulkD"="Bulk Density","Clay"="Clay", "Sand" = "Sand", "Silt" = "Silt")

# vector de profundidades
depth <- c("0-5cm","5-15cm","15-30cm")

# Definir zonas climáticas
# Hypersemi-arid 17°-32°
# Mediterranean 32°-43°
# Rainy and Patagonia 43°-56°
hyperarid <- ext(c(-78, -65, -32, -17))
mediterranean <- ext(c(-78, -65, -43, -32))
patagonia <- ext(c(-78, -65, -56, -43))

sf.arid <- hyperarid
sf.medi <- mediterranean
sf.pata <- patagonia

ugeo.sf <- st_as_sf(ugeo);ugeo.sf

unidades.names <- c("Coastal Plains", "Coastal Range", "Central Valley","Serranias","Pre-Andean Range","Andes Range","Extrandean Patagonia")
clases <- factor(unidades.names, levels = unidades.names)
clases
# clases <- factor(ugeo.sf$r_name_eng);clases
colpal <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03")
colpal = c('#E3170A','#2B4162','#88D18A','#F3B700','#FF66D8','#22031F','#0090C1')
colScale <- scale_fill_manual(values = rev(colpal), limits = levels(clases), drop = F)
colScale.color <- scale_color_manual(values = rev(colpal), limits = levels(clases), drop = F)


map.plot <- ggplot(data = ugeo.sf) +
  geom_sf(aes(fill = r_name_eng),color = NA, alpha = 0.8) +
  # geom_sf(data = sf.arid, alpha = 0, color = "black") +
  # geom_sf(data = sf.medi, alpha = 0, color = "black") +
  # geom_sf(data = sf.pata, alpha = 0, color = "black") +
  geom_hline(yintercept =-17, col = "red", linetype = "dashed", size = 1)+
  geom_hline(yintercept =-32, col = "red", linetype = "dashed", size = 1)+
  geom_hline(yintercept =-43, col = "red", linetype = "dashed", size = 1)+
  geom_hline(yintercept =-56, col = "red", linetype = "dashed", size = 1)+
  
  annotate("text",x = -66, y = -24.5, label = "Hyper/Semi Arid Zone", hjust = 0.5,vjust = 1.6, angle = 90)+
  annotate("text",x = -66, y = -37.5, label = "Mediterranean Zone", hjust = 0.5, vjust = 1.6, angle = 90)+
  annotate("text",x = -66, y = -49.5, label = "Rainy and Patagonia Zone", hjust = 0.5, vjust = 1.6, angle = 90)+
  
  annotate("text",x = -76, y = -17, label = "17ºS", hjust = 1.2,vjust = 0.5, angle = 0, color = "red", size = 3.9)+
  annotate("text",x = -76, y = -32, label = "32ºS", hjust = 1.2,vjust = 0.5, angle = 0, color = "red", size = 3.9)+
  annotate("text",x = -76, y = -43, label = "43ºS", hjust = 1.2,vjust = 0.5, angle = 0, color = "red", size = 3.9)+
  annotate("text",x = -76, y = -56, label = "56ºS", hjust = 1.2,vjust = 0.5, angle = 0, color = "red", size = 3.9)+
  
  theme(legend.position = "none")+
  coord_sf(clip = "off")+
  colScale +
  labs(x = "",y = "")
map.plot

plot2.list <- list()
plot2.cont <- 1
for (i in 1:4){
  # i = 2
  var <- variables[i]
  cat("Variable: ", var, "\n")
  # cargar mapas de una variable
  maps <- soil.maps[[grep(var, names(soil.maps))]] [[c(1,5,3)]];names(maps)
  m <- mean(maps)
  m
  if (var == "BulkD"){
    bandwith  = 0.1
    # lab.x = "Geomorphologic Unit"
    y.lim = ylim(0,2.6)
  } else if (var == "Clay"){
    bandwith = 1
    # lab.x = ""
    y.lim = ylim(0,75)
  } else if (var == "Sand"){
    bandwith = 3
    # lab.x = ""
    y.lim = ylim(0,95)
  } else if (var == "Silt"){
    bandwith = 1
    # lab.x = ""
    y.lim = ylim(0,75)
  }
  
  cat("Creating PLOT for\n", var.names[var], "\n")
  m.arid <- terra::crop(m, hyperarid)
  m.medi <- terra::crop(m, mediterranean)
  m.pata <- terra::crop(m, patagonia)
  
  
  m.val.arid <- terra::extract(m.arid, ugeo)
  m.val.medi <- terra::extract(m.medi, ugeo)
  m.val.pata <- terra::extract(m.pata, ugeo)
  
  m.val.arid$zone = "Hyper/Semi Arid Zone"
  m.val.medi$zone = "Mediterranean Zone"
  m.val.pata$zone = "Rainy and Patagonia Zone"
  
  m.val <- rbind(m.val.arid, m.val.medi, m.val.pata)
  rm(m.val.arid,m.val.medi,m.val.pata)
  m.val$UNIDAD <- ugeo$r_name_eng[match(m.val$ID, ugeo$ID)]
  names(m.val)[2] <- "value"
  m.val
  
  ###
  # Ajustar escala para que todas sean iguales.
  # Dejar solo el nombre del eje y para los de densidad aparente
  # Dejar solo el titulo del primer grafico de cada propiedad
  temp.plot1 <- m.val %>% filter(zone == "Hyper/Semi Arid Zone") %>% drop_na() %>% 
    ggplot(aes(x = UNIDAD, y = value)) +
    geom_violin(aes(fill = UNIDAD, color = UNIDAD), trim = F, bw = bandwith, alpha = 0.8) +
    geom_boxplot(width = 0.07, outlier.shape = NA) +
    colScale +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank()) +
    labs(x = "", y = "", title = var.names[var]) +
    colScale.color +
    scale_x_discrete(limits = clases[-7]) +
    y.lim
  temp.plot1
  # coord_flip()
  # temp.plot1
  temp.plot2 <- m.val %>% filter(zone == "Mediterranean Zone") %>% drop_na() %>% 
    ggplot(aes(x = UNIDAD, y = value)) +
    geom_violin(aes(fill = UNIDAD, color = UNIDAD), trim = F, bw = bandwith, alpha = 0.8, color = NA) +
    geom_boxplot(width = 0.07, outlier.shape = NA) +
    colScale +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank()) +
    labs(x = "", y = "") +
    colScale.color +
    scale_x_discrete(limits = clases[-c(5,7)]) +
    y.lim
  # coord_flip()
  # temp.plot2
  temp.plot3 <- m.val %>% filter(zone == "Rainy and Patagonia Zone") %>% drop_na() %>% 
    ggplot(aes(x = UNIDAD, y = value)) +
    geom_violin(aes(fill = UNIDAD, color = UNIDAD), trim = F, bw = bandwith, alpha = 0.8, color = NA) +
    geom_boxplot(width = 0.07, outlier.shape = NA) +
    colScale +
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks = element_blank()) +
    labs(x = "Geomorphologic Unit", y = "") +
    colScale.color +
    scale_x_discrete(limits = clases[-c(1,4,5)]) +
    y.lim
  # coord_flip()
  # temp.plot3
  clases

  # plot2.cont <- 1
  plot2.list[[plot2.cont]] <- temp.plot1
  plot2.cont = plot2.cont + 1
  plot2.list[[plot2.cont]] <- temp.plot2
  plot2.cont = plot2.cont + 1
  plot2.list[[plot2.cont]] <- temp.plot3
  plot2.cont = plot2.cont + 1
}
grob <- ggplotGrob(ggplot(m.val, aes(x = UNIDAD, y = value)) +
                     geom_violin(aes(fill = UNIDAD), trim = F, bw = bandwith, alpha = 0.8) +
                     colScale +
                     guides(fill = guide_legend(title = "Geomorphologic Unit")))
id.legend <- grep("guide", grob$layout$name)
legend.plot2 <- grob[["grobs"]][[id.legend]]
# plot(legend.plot2)
# PLOT 2
plot2.list[[13]] <- legend.plot2
plot2.list[[14]] <- map.plot
lay <- rbind(c(14,1,4,7,10,13),
             c(14,2,5,8,11,13),
             c(14,3,6,9,12,13))
beepr::beep(sound = 4)

grid.arrange(grobs = plot2.list, layout_matrix = lay, legend)
######################## FIN PARTE 4 #####################
#####-----------------------------------------------######
############# PARTE 5 (NOT UPDATED): METRICAS DE DISTRIBUCION POR UGEO Y ZONA CLIMATICA ##########
# area de estudio para la projeccion
ae <- rast("./materiales/area_estudio/ae_buffer_0.001deg.tif")

# Cargar unidades geomorfologicas
ugeo <- st_read("./materiales/unidades_geomorfologicas/unidades_geomorfologicas.shp", crs = 4326)
ugeo.csv <- read_csv("./materiales/unidades_geomorfologicas/ugeo.csv")
ugeo$ID = seq(1:nrow(ugeo)) #agregar columna ID
ugeo <- as(ugeo, "SpatVector")

# Cargar mapas de propiedades de suelos
files <- list.files("D:/paper_BDsuelos/Versiones/Version3/post_procesado/100m_v3", pattern = ".tif$", full.names = T)
files <- files[grep("Arcilla|Da|Arena|Limo", files)]
soil.maps <- files %>% rast
names(soil.maps)
# soil.maps <- project(soil.maps, "epsg:4326")
variables <- c("Clay.","Sand.","BulkD.","Silt.")
depths <- c("0-5cm","100-200cm","15-30cm","30-60cm","5-15cm","60-100cm")
f.names<-lapply(variables, function(v){paste0(v,depths)}) %>% unlist;f.names
names(soil.maps) <- f.names
soil.names <- names(soil.maps);soil.names


# vector de variables 
variables <- c("BulkD","Clay","Sand","Silt")
var <- variables[1]
var.names <- c("BulkD"="Bulk Density","Clay"="Clay", "Sand" = "Sand","Silt" = "Silt")

# vector de profundidades
depth <- c("0-5cm","5-15cm","15-30cm")

# Definir zonas climáticas
# Hypersemi-arid 17°-32°
# Mediterranean 32°-43°
# Rainy and Patagonia 43°-56°
hyperarid <- ext(c(-78, -65, -32, -17))
mediterranean <- ext(c(-78, -65, -43, -32))
patagonia <- ext(c(-78, -65, -56, -43))

sf.arid <- hyperarid
sf.medi <- mediterranean
sf.pata <- patagonia

ugeo.sf <- st_as_sf(ugeo);ugeo.sf


clases <- factor(ugeo.sf$r_name_eng);clases
colpal <- c("#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03")
colScale <- scale_fill_manual(values = colpal, limits = levels(clases), drop = F)
colScale.color <- scale_color_manual(values = colpal, limits = levels(clases), drop = F)


north.z <- "Hyper/Semi Arid Zone"
center.z <- "Mediterranean Zone"
south.z <- "Rainy and Patagonia Zone"

metrics.df <- tibble()

for (i in 1:4){
  # i = 1
  var <- variables[i]
  cat("Variable: ", var, "\n")
  # cargar mapas de una variable
  maps <- soil.maps[[grep(var, names(soil.maps))]] [[c(1,5,3)]]
  m <- mean(maps)
  
  cat("Cropping and extracting by soil-climate zone for\n", var.names[var], "\n")
  m.arid <- terra::crop(m, hyperarid)
  m.medi <- terra::crop(m, mediterranean)
  m.pata <- terra::crop(m, patagonia)
  
  m.val.arid <- terra::extract(m.arid, ugeo)
  m.val.medi <- terra::extract(m.medi, ugeo)
  m.val.pata <- terra::extract(m.pata, ugeo)
  
  m.val.arid$zone = "Hyper/Semi Arid Zone"
  m.val.medi$zone = "Mediterranean Zone"
  m.val.pata$zone = "Rainy and Patagonia Zone"
  
  m.val <- rbind(m.val.arid, m.val.medi, m.val.pata)
  rm(m.val.arid,m.val.medi,m.val.pata)
  m.val$UNIDAD <- ugeo$r_name_eng[match(m.val$ID, ugeo$ID)]
  names(m.val)[2] <- "value"
  
  ###
  
  metrics.df <- tibble(m.val) %>% 
    group_by(zone, UNIDAD) %>% 
    summarise(mean_val = mean(value, na.rm = T) %>% round(digits = 2),
              median_val = median(value, na.rm = T) %>% round(digits = 2),
              sd = sd(value, na.rm  = T) %>% round(digits = 2),
              maximum = max(value, na.rm = T) %>% round(digits = 2),
              minimum = min(value, na.rm = T) %>% round(digits = 2),
              q0.5 = quantile(value, 0.25, na.rm = T) %>% round(digits = 2),
              q0.75 = quantile(value, 0.75, na.rm = T) %>% round(digits = 2),
              IQR = IQR(value, na.rm=TRUE) %>% round(digits = 2)) %>% 
    mutate(soil_attribute = var) %>% bind_rows(metrics.df)
}
beepr::beep(sound = 5)
# metrics.df %>% pivot_wider(names_from = "soil_attribute", id_cols = 3:6)
metrics.df %>% drop_na() %>% write_csv("./plots/estadistica_por_geozona.csv")
######################## FIN PARTE 5 #####################
#####-----------------------------------------------######
############# PARTE 6: RESUMEN ESTADISTICO POR PROPIEDAD EN LA BD #############################################

bd <- read_csv(here("data","soil_database","BD_soilprof_23MAR.csv"))
tex <- bd %>% filter(Arcilla != -999)
da <- bd %>% filter(Da != -999)
clay <- tex %>% dplyr::select(ID, val = Arcilla)
sand <- tex %>% dplyr::select(ID, val = arena)
silt <- tex %>% dplyr::select(ID, val = Limo)
da <- da %>% dplyr::select(ID, val = Da)
cc = filter(bd, CC != -999) %>% select(ID, val = CC)
pmp = filter(bd, PMP !=-999) %>% select(ID, val = PMP)

# Resumen estadistico por propiedad
{
  nprof <- clay %>% group_by(ID) %>% slice(1) %>% nrow
  df1 <- clay %>% summarise(max = max(val),
                            min = min(val),
                            promedio = mean(val),
                            mediana = median(val),
                            desv = sd(val),
                            total = n()) %>% 
    mutate(n_profiles = nprof, propiedad = "Clay")
  
  nprof <- sand %>% group_by(ID) %>% slice(1) %>% nrow
  df2 <- sand %>% summarise(max = max(val),
                            min = min(val),
                            promedio = mean(val),
                            mediana = median(val),
                            desv = sd(val),
                            total = n())%>% 
    mutate(n_profiles = nprof, propiedad = "Sand")
  
  nprof <- silt %>% group_by(ID) %>% slice(1) %>% nrow
  df3 <- silt %>% summarise(max = max(val),
                            min = min(val),
                            promedio = mean(val),
                            mediana = median(val),
                            desv = sd(val),
                            total = n())%>% 
    mutate(n_profiles = nprof, propiedad = "Silt")
  
  nprof <- da %>% group_by(ID) %>% slice(1) %>% nrow
  df4 <- da %>% summarise(max = max(val),
                          min = min(val),
                          promedio = mean(val),
                          mediana = median(val),
                          desv = sd(val),
                          total = n())%>% 
    mutate(n_profiles = nprof, propiedad = "Bulk Density")
  nprof <- cc %>% group_by(ID) %>% slice(1) %>% nrow
  df5 <- cc %>% summarise(max = max(val),
                          min = min(val),
                          promedio = mean(val),
                          mediana = median(val),
                          desv = sd(val),
                          total = n())%>% 
    mutate(n_profiles = nprof, propiedad = "Field Cap.")
  df6 <- pmp %>% summarise(max = max(val),
                          min = min(val),
                          promedio = mean(val),
                          mediana = median(val),
                          desv = sd(val),
                          total = n())%>% 
    mutate(n_profiles = nprof, propiedad = "Perm. Wilt. Point")
}
bd_summary <- bind_rows(df1,df2,df3,df4,df5,df6)
bd_summary %>% write_csv(here("results","tables","summary_bd.csv"))

######################## FIN PARTE 6 #####################
#####-----------------------------------------------######
############# PARTE 7: CANTIDAD PUNTOS POR FUENTE #############################################

bd <- read_csv(here("data","soil_database","BD_soilprof_23MAR.csv"))
bd.p = bd %>% select(ID, Fuente, Arcilla, PMP, CC, Da) %>% pivot_longer(cols = Arcilla:Da, names_to = "soil_att")
bd.p %>% filter(value != -999) %>%   mutate(Fuente = if_else(Fuente == "Otros", "UChile",Fuente)) %>% 
  group_by(Fuente, soil_att) %>% 
  summarise(count = n())
  
bd.p %>% filter(value != -999) %>%  
  mutate(Fuente = if_else(Fuente == "Otros", "UChile",Fuente)) %>% 
  group_by(ID, Fuente, soil_att) %>%
  slice(1) %>% 
  ungroup() %>% 
  group_by(Fuente, soil_att) %>% 
  summarise(count = n()) %>% 
  pivot_wider(names_from = 'soil_att', values_from = 'count')

bd.p %>% filter(value != -999) %>%  
  mutate(Fuente = if_else(Fuente == "Otros", "UChile",Fuente)) %>% 
  group_by(Fuente, soil_att) %>%
  summarise(count = n()) %>% 
  pivot_wider(names_from = 'soil_att', values_from = 'count')


bd.p %>% Hmisc::describe()
bd.p %>% skimr::skim()

bd <- bd %>% filter(Arcilla != -999 | Da != -999)
bd %>% group_by(Fuente) %>% summarise(n = n()) %>% 
  write_csv(here("results","tables","Puntos_por_fuente.csv"))

bd.p = bd %>% select(ID, x,y, Fuente, Arcilla, Da, CC, PMP) %>% pivot_longer(cols = Arcilla:PMP, names_to = "soil_att")
bd.p %>% filter(value != -999) %>%
  mutate(Fuente = if_else(Fuente == "Otros", "UChile",Fuente)) %>% 
  group_by(ID, Fuente, soil_att) %>%
  slice(1) %>% 
  ungroup() %>%
  pivot_wider(names_from = "soil_att", values_from = "value") %>% 
  write_csv(file = here("results","tables","BD_para_cartografia.csv"))

######################## FIN PARTE 7 #####################
#####-----------------------------------------------######
############# PARTE 8: MAPAS 0-5cm ######################

# load soil maps
dir <- here("results","postproc","v1","SoilMaps_MEAN")
list.files(dir)
files <- list.files(dir, pattern = ".tif$", full.names = T);files
files <- files[grep(files, pattern = "5-15")];files
bulkd <- raster(files[1])
clay <- raster(files[2])
sand <- raster(files[3])
silt <- raster(files[4])

theme_set(theme_bw())
colpal <- rev(scico::scico(n = 7, palette = "roma"))

plot.list <- list()
plot.list[[1]] <- gplot(clay) +
  geom_raster(aes(fill = value), na.rm = T, interpolate = T) +
  scale_fill_gradientn(colours = colpal, limits = c(0,60), na.value = NA, name = "Clay content [%]") +
  coord_equal() +
  labs(x = "", y = "", title = "A) Clay 5-15cm")

plot.list[[2]] <-  gplot(sand) +
  geom_raster(aes(fill = value), na.rm = T, interpolate = T) +
  scale_fill_gradientn(colours = colpal, limits = c(10,90), na.value = NA, name = "Sand content [%]") +
  coord_equal() +
  labs(x = "", y = "", title = "B) Sand 5-15cm")

plot.list[[3]] <- gplot(bulkd) +
  geom_raster(aes(fill = value), na.rm = T, interpolate = T) +
  scale_fill_gradientn(colours = colpal, limits = c(0.15,2.1), na.value = NA, name = "Bulk density [g/cm3]") +
  coord_equal() +
  labs(x = "", y = "", title = "C) Bulk Density 5-15cm")

plot.list[[4]] <- gplot(silt) +
  geom_raster(aes(fill = value), na.rm = T, interpolate = T) +
  scale_fill_gradientn(colours = colpal, limits = c(0,60), na.value = NA, name = "Silt content [%]") +
  coord_equal() +
  labs(x = "", y = "", title = "D) Silt 5-15cm")

for (i in 1:4) {
  x = c("clay", "sand","bulkd","silt")
  ggsave(plot.list[[i]], filename = here("results","figures",
                                         paste0(x[i],"_CLSM_100M_5-15cm.eps")), width = 4, height = 6)
}


lay <- rbind(c(1,2,3))
g <- arrangeGrob(grobs = plot.list[c(1,2,3)], layout_matrix = lay)
ggsave(plot = g, filename = here("results","figures","CLSM_100M_5-15cm.eps"), width = 10, height = 10)
grid.arrange(grobs = plot.list, layout_matrix = lay)


lay <- rbind(c(1,2),
             c(3,4))
# lay <- rbind(c(1,2,4,3))
g <- arrangeGrob(grobs = plot.list, layout_matrix = lay)
ggsave(plot = g, filename = here("results","figures","CLSM_100M_5-15cm_wsilt.eps"), width = 4, height = 6)
grid.arrange(grobs = plot.list, layout_matrix = lay)

######################## FIN PARTE 8 #####################
#####-----------------------------------------------######
############# PARTE 9: PICP PLOTS ########################
variables <- c("Da","Arcilla","arena")
selection_clay <- paste0(c("upper","upper","lower","lower","lower","upper"),"_uncor")
selection_sand <- paste0(c("upper","upper","upper","upper","upper","upper"),"_uncor")
selection_bulkd <- paste0(c("upper","upper","upper","upper","lower","lower"),"_uncor")

depths.nam <- c("2.5" = "0-5 cm" ,"10" = "5-15 cm", "22.5" = "15-30 cm","45" = "30-60 cm","80" = "60-100 cm","150" = "100-200 cm")
depths <- c(2.5, 10, 22.5, 45, 80, 150)
var.eng <- c(Da = "Bulk Density", Arcilla = "Clay", Limo = "Silt", arena = "Sand")
sel.nam <- c("upper_uncor" = "U", "lower_uncor" = "L")

# vector of quantiles
qp <- qnorm(c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6,
              0.55, 0.525))
# percentiles
cs <- c(99, 97.5, 95, 90, 80, 60, 40, 20, 10, 5)

#Selecion de modelos por profundidad
p.list <- list()
m.list <- list()
bound.list <- list()
plot.list <- list()
for (k in 1:3){
  var <- variables[k]
  cat("Comenzando con ", var, "\n")
  
  for (i in 1:6) {
    if (var == "Arcilla"){
      sel <- selection_clay[i]
    }
    if (var == "arena"){
      sel <- selection_sand[i]
    }
    if (var == "Da"){
      sel <- selection_bulkd[i]
    }
    results <- read_rds(here("results","models",str_c("RF_",var,"_",sel,"_results.rds")))
    names(results) <- c("bootstrap","validation")
    val.results <- results$validation
    metrics <- lapply(val.results, function(l) l$metrics) %>% rlist::list.rbind(); metrics %>% head()
    predictions <- lapply(val.results, function(l) l$predictions) %>% rlist::list.rbind();predictions
    
    pred.stats <- get_pred_stats(var, "RF", predictions); pred.stats
    avg.MSE <- metrics %>% group_by(depth) %>% 
      summarise(avgMSE = mean(MSE))
    
    # juntar tablas
    pred.stats <- left_join(pred.stats, avg.MSE)
    pred.stats <- pred.stats %>% mutate(sd.var = sqrt(pred.var + avgMSE))
    pred.stats
    # upper y lower limits of prediction interval
    upper_lim <- map(qp, function(q){pred.stats$pred.mean + pred.stats$sd.var * q}) %>% 
      rlist::list.cbind()
    upper_lim <- bind_cols(upper_lim, depth =  pred.stats$depth)
    
    lower_lim <- map(qp, function(q){pred.stats$pred.mean - pred.stats$sd.var * q}) %>%
      rlist::list.cbind()
    lower_lim <- bind_cols(lower_lim, depth = pred.stats$depth)
    
    names(upper_lim)[1:10] <- paste0("q",cs)
    names(lower_lim)[1:10] <- paste0("q",cs)
    
    # Contar valores que caen dentro del PI
    bound_Mat <- matrix(NA, nrow = nrow(pred.stats), ncol = length(cs))
    for (j in 1:length(cs)){
      bound_Mat[ ,j] <- as.numeric(pred.stats$obs >= lower_lim[ ,j] & pred.stats$obs <= upper_lim[ ,j] )
    }
    bound_Mat <- bound_Mat %>% bind_cols(depth = pred.stats$depth)
    names(bound_Mat)[1:10] <- paste0("q",cs)
    bound_Mat
    
    
    p.list[[i]] <- pred.stats %>% 
      filter(depth == depths[i]) %>% 
      dplyr::select(pred.mean, obs, error, depth) %>%
      mutate(modelo = paste0("RF-",sel.nam[sel]))
    m.list[[i]] <- metrics %>%
      filter(depth == depths[i]) %>%
      dplyr::select(depth, ME, PBIAS = PBIAS.., RMSE, NRMSE = NRMSE.., r, R2) %>%
      mutate(modelo = paste0("RF-", sel.nam[sel]))
    bound.list[[i]] <- bound_Mat %>%
      filter(depth == depths[i]) %>%
      mutate(modelo = paste0("RF-", sel.nam[sel]))
    
  }
  
  pred.stats <- rlist::list.rbind(p.list)
  metrics.stats <- rlist::list.rbind(m.list)
  bound.PICP <- rlist::list.rbind(bound.list)
  
  
  group.sum <- bound.PICP %>% group_by(depth) %>% 
    summarise_if(is.numeric, sum)
  
  group.tot <- bound.PICP %>% group_by(depth) %>% 
    summarise_if(is.numeric, length)
  
  
  picp.model <- bound.PICP %>% group_by(depth) %>% slice(1) %>% dplyr::select(modelo)
  picp.names <- map2(picp.model$modelo, depths.nam, function(p, d){str_c(p, " : ", d)}) %>% unlist
  names(picp.names) <- depths;picp.names
  
  # calcular PICP
  picp <- group.sum/group.tot;picp
  picp %>% dplyr::select(2:11) %>%
    mutate(depth = round(group.sum$depth, digits = 2)) %>%
    write_csv(here("results","tables",str_c("picp_",var,".csv")))
  
  
  picp <- picp %>% dplyr::select(2:11) %>% mutate(depth = group.sum$depth) %>% 
    pivot_longer(cols = 1:10, names_to = 'quantile', values_to = 'q.value') %>% 
    mutate(quantile = rep(cs, 6), q.value = q.value*100);picp
  
  # plot PICP
  if (var == "Da"){
    p.plot <- picp %>% ggplot(aes(x = quantile, y = q.value)) +
      geom_point()+
      geom_abline(slope = 1, intercept = 0, color = "red") +
      facet_wrap(~depth, labeller = labeller(depth = picp.names)) +
      coord_fixed(xlim = c(0,100), ylim = c(0,100)) +
      labs( x = "Confidence level %", y = "PICP", title = "A) Bulk Density")
  }
  if (var == "Arcilla"){
    p.plot <- picp %>% ggplot(aes(x = quantile, y = q.value)) +
      geom_point()+
      geom_abline(slope = 1, intercept = 0, color = "red") +
      facet_wrap(~depth, labeller = labeller(depth = picp.names)) +
      coord_fixed(xlim = c(0,100), ylim = c(0,100)) +
      labs( x = "Confidence level %", y = "PICP", title = "B) Clay")
  }
  if (var == "arena"){
    p.plot <- picp %>% ggplot(aes(x = quantile, y = q.value)) +
      geom_point()+
      geom_abline(slope = 1, intercept = 0, color = "red") +
      facet_wrap(~depth, labeller = labeller(depth = picp.names)) +
      coord_fixed(xlim = c(0,100), ylim = c(0,100)) +
      labs( x = "Confidence level %", y = "PICP", title = "C) Sand")
  }
  p.plot
  # p2.plot <- picp %>% ggplot(aes(x = quantile, y = q.value)) +
  #   geom_point()+
  #   geom_abline(slope = 1, intercept = 0, color = "red") +
  #   facet_wrap(~depth, labeller = labeller(depth = depths.nam)) +
  #   coord_fixed(xlim = c(0,100), ylim = c(0,100)) +
  #   labs( x = "Nivel de confianza", y = "PICP")
  # p2.plot
  
  plot.list[[k]] <- p.plot
  ggsave(filename = here::here("results", "figures", str_c("PICP_", var,".png")), plot = p.plot, width = 6, height = 5)
  ggsave(filename = here::here("results", "figures", str_c("PICP_", var,".eps")), plot = p.plot, width = 6, height = 5)
  
}

xlay <- rbind(c(1,1,2,2),
             c(4,3,3,4))
grid.arrange(plot.list[[1]], plot.list[[2]], plot.list[[3]], layout_matrix = xlay)
p.plot = arrangeGrob(plot.list[[1]], plot.list[[2]], plot.list[[3]], layout_matrix = xlay)
ggsave(filename = here::here("results","figures","PICP.png"), width = 9, height = 7, p.plot)
ggsave(filename = here::here("results","figures","PICP.eps"), width = 9, height = 7, p.plot)

######################## FIN PARTE 9 #####################
#####-----------------------------------------------######
############# PARTE 10: METRICS PLOTS #####################

variables <- c("Da","Arcilla","arena")
selection_clay <- paste0(c("upper","upper","lower","lower","lower","upper"),"_uncor")
selection_sand <- paste0(c("upper","upper","upper","upper","upper","upper"),"_uncor")
selection_bulkd <- paste0(c("upper","upper","upper","upper","lower","lower"),"_uncor")

depths.nam <- c("2.5" = "0-5 cm" ,"10" = "5-15 cm", "22.5" = "15-30 cm","45" = "30-60 cm","80" = "60-100 cm","150" = "100-200 cm")
depths <- c(2.5, 10, 22.5, 45, 80, 150)
var.eng <- c(Da = "Bulk Density", Arcilla = "Clay", Limo = "Silt", arena = "Sand")
sel.nam <- c("upper_uncor" = "U", "lower_uncor" = "L")
# vector of quantiles

#Selecion de modelos por profundidad
m.list <- list()
metrics.plots <- list()
for (k in 1:3){
  var <- variables[k]
  cat("Comenzando con ", var, "\n")
  for (i in 1:6) {
  if (var == "Arcilla") {
    sel <- selection_clay[i]
  }
  if (var == "arena") {
    sel <- selection_sand[i]
  }
  if (var == "Da") {
    sel <- selection_bulkd[i]
  }
  results <- read_rds(here("results", "models", str_c("RF_", var, "_", sel, "_results.rds")))
  names(results) <- c("bootstrap", "validation")
  val.results <- results$validation
  metrics <- lapply(val.results, function(l) l$metrics) %>% rlist::list.rbind()
  metrics %>% head()
  predictions <- lapply(val.results, function(l) l$predictions) %>% rlist::list.rbind()
  predictions

  m.list[[i]] <- metrics %>%
    filter(depth == depths[i]) %>%
    dplyr::select(depth, ME, PBIAS = PBIAS.., RMSE, NRMSE = NRMSE.., r, R2) %>%
    mutate(modelo = paste0("RF-", sel.nam[sel]))
}
  
  metrics.stats <- rlist::list.rbind(m.list)
  metrics.stats %>% group_by(depth, modelo) %>% 
    summarise(R2_mean = mean(R2),
              RMSE_mean = mean(RMSE),
              NRMSE_mean = mean(NRMSE),
              PBIAS_mean = mean(PBIAS)) %>% 
    write_csv(here("results","tables",paste0(var, "_validations_metrics_mean.csv")))
  
  variable.labels <- c("PBIAS",expression(R^2),"RMSE")
  variable.labels <- c(expression(R^2),"RMSE")
  
  # plot metrics
  if (var == "Da"){
    m.plot <-  metrics.stats %>%  pivot_longer(cols = c("RMSE","R2"), names_to = "metric", values_to = "value") %>%
      mutate(depth = as_factor(depth),
             metric = factor(metric, labels = variable.labels)) %>% 
      ggplot() +
      geom_violin(aes(x = depth, y = value), trim = F, show.legend = F, bw = 0.005) +
      geom_boxplot(aes(x = depth, y = value), show.legend = F, width = 0.1, outlier.alpha = 0)+
      facet_wrap(~ metric, scales = "free", labeller = "label_parsed") +
      scale_x_discrete(labels = str_sub(depths.nam, start = 1, end = nchar(depths.nam)-3)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), strip.text = element_text(size = 12, face = "bold")) +
      labs(x = "Depth [cm]", y = "", title = "A) Bulk Density")
  }
  m.plot
  if (var == "Arcilla"){
    m.plot <-  metrics.stats %>%  pivot_longer(cols = c("RMSE","R2"), names_to = "metric", values_to = "value") %>%
      mutate(depth = as_factor(depth),
             metric = factor(metric, labels = variable.labels)) %>% 
      ggplot() +
      geom_violin(aes(x = depth, y = value), trim = F, show.legend = F) +
      geom_boxplot(aes(x = depth, y = value), show.legend = F, width = 0.1, outlier.alpha = 0)+
      facet_wrap(~ metric, scales = "free", labeller = "label_parsed") +
      scale_x_discrete(labels = str_sub(depths.nam, start = 1, end = nchar(depths.nam)-3)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), strip.text = element_text(size = 12, face = "bold")) +
      labs(x = "Depth [cm]", y = "", title = "B) Clay")
    m.plot
  }
  if (var == "arena"){
    m.plot <-  metrics.stats %>%  pivot_longer(cols = c("RMSE","R2"), names_to = "metric", values_to = "value") %>%
      mutate(depth = as_factor(depth),
             metric = factor(metric, labels = variable.labels)) %>% 
      ggplot() +
      geom_violin(aes(x = depth, y = value), trim = F, show.legend = F) +
      geom_boxplot(aes(x = depth, y = value), show.legend = F, width = 0.1, outlier.alpha = 0)+
      facet_wrap(~ metric, scales = "free", labeller = "label_parsed") +
      scale_x_discrete(labels = str_sub(depths.nam, start = 1, end = nchar(depths.nam)-3)) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), strip.text = element_text(size = 12, face = "bold")) +
      labs(x = "Depth [cm]", y = "", title = "C) Sand")
    m.plot
  }
  m.plot
  ggsave(filename = here("results","figures",str_c("METRICS_",var,".png")), height = 5, width = 6, plot = m.plot)
  ggsave(filename = here("results","figures",str_c("METRICS_",var,".eps")), height = 5, width = 6, plot = m.plot)
  
  # p2.plot <- picp %>% ggplot(aes(x = quantile, y = q.value)) +
  #   geom_point()+
  #   geom_abline(slope = 1, intercept = 0, color = "red") +
  #   facet_wrap(~depth, labeller = labeller(depth = depths.nam)) +
  #   coord_fixed(xlim = c(0,100), ylim = c(0,100)) +
  #   labs( x = "Nivel de confianza", y = "PICP")
  # p2.plot
  
  metrics.plots[[k]] <- m.plot
}

lay <- rbind(c(1,1,2,2),
             c(4,3,3,4))
grid.arrange(metrics.plots[[1]], metrics.plots[[2]], metrics.plots[[3]], layout_matrix = lay)
m.plot = arrangeGrob(metrics.plots[[1]], metrics.plots[[2]], metrics.plots[[3]], layout_matrix = lay)
ggsave(filename = here("results","figures","METRICS.png"), height = 7, width = 10, plot = m.plot)
ggsave(filename = here("results","figures","METRICS.eps"), height = 7, width = 10, plot = m.plot)


######################## FIN PARTE 10 ####################
#####-----------------------------------------------######
############# PARTE 11: MAPAS 5-15, 30-60, 100-200cm Y MAPAS DE SUMPLEMENTO ######################

# cargar mapas de suelo
dir <- here("results","postproc","v1","SoilMaps_MEAN")
variables_eng = c("Bulkd","Clay","Sand","Silt")
variables_esp = c("Da","Arcilla","Arena","Limo")
list.files(dir)

# Profundidades que van en el texto
depths <- c("5-15","30-60","100-200")

# Profundidades que van en el suplemento
depths.sup <- c("0-5","15-30","60-100")

# All depths
depths.all <- c("0-5","5-15","15-30","30-60","60-100","100-200")

# n = 1
plot.list <- list()
for (i in 1:4) {
  # i = 1
  # i=3
  var_eng <- variables_eng[i]
  var_esp <- variables_esp[i]
  files <- list.files(dir, pattern = ".tif$", full.names = T);files
  files.var <- files[grep(pattern = var_eng, x = files)];files.var
  
  index <- lapply(depths, function(d){grep(pattern = d, files.var)}) %>% unlist;index
  imgs <- stack(files.var[index]);imgs
  var_title = c("A) Bulk Density", "B) Clay",  "C) Sand", "D) Silt")
  names(imgs) <- paste0(depths,"cm")
  if (i == 1){breaks = seq(0.1,2.3,0.22)}
  if (i == 2){breaks = seq(0,70,10)}
  if (i == 3){breaks = seq(0,100,10)}
  if (i == 4){breaks = seq(0,90,10)}
  colpal <- c("#001219","#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03","#ae2012","#9b2226")
  my.settings <- list(
    par.main.text = list(font = 2, # make it bold
                         just = "left", 
                         x = grid::unit(5, "mm")))
  
  lvlp <- rasterVis::levelplot(imgs, col.regions = colpal, at = breaks, main = var_title[i], par.settings=my.settings, names.attr = paste0(depths,"cm"));lvlp
  png(filename = here("results","figures", str_c(var_eng,"_depths_1.png")), width = 600, height = 800, units = "px")
  print(lvlp)
  dev.off()
  plot.list[[i]] <- lvlp
}
plot.list[[4]]
lay <- rbind(c(1,2),
             c(3,4))
g <- arrangeGrob(grobs = plot.list, layout_matrix = lay)
ggsave(plot = g, filename = here("results","figures","CR2SOILS_physical_att.png"), width = 10, height = 12)
# grid.arrange(grobs = plot.list, layout_matrix = lay)

for (i in 1:4) {
  # i = 2
  # i = 1
  var_eng <- variables_eng[i]
  var_esp <- variables_esp[i]
  
  files <- list.files(dir, pattern = ".tif$", full.names = T);files
  files.var <- files[grep(pattern = var_eng, x = files)];files.var
  
  index <- lapply(depths.sup, function(d){grep(pattern = d, files.var)}) %>% unlist;index
  imgs <- stack(files.var[index]);imgs
  var_title = c("A) Bulk Density", "B) Clay",  "C) Sand", "D) Silt")
  if (i == 1){breaks = seq(0.1,2.3,0.22)}
  if (i == 2){breaks = seq(0,70,10)}
  if (i == 3){breaks = seq(0,100,10)}
  if (i == 4){breaks = seq(0,90,10)}
  colpal <- c("#001219","#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03","#ae2012","#9b2226")
  
  # Ajustar titulo en latticle plots, source: https://www.magesblog.com/post/2015-06-16-how-to-place-titles-in-lattice-plots/
  my.settings <- list(
    par.main.text = list(font = 2, # make it bold
                         just = "left", 
                         x = grid::unit(5, "mm")))
  
  lvlp <- rasterVis::levelplot(imgs, col.regions = colpal, at = breaks, main = var_title[i], par.settings=my.settings, names.attr = paste0(depths.sup,"cm"));lvlp
  plot.list[[i]] <- lvlp
  png(filename = here("results","figures", str_c(var_eng,"_depths_2.png")), width = 600, height = 800, units = "px")
  print(lvlp)
  dev.off()
}

plot.list[[4]]
lay <- rbind(c(1,2),
             c(3,4))
g <- arrangeGrob(grobs = plot.list, layout_matrix = lay)
ggsave(plot = g, filename = here("results","figures","CR2SOILS_physical_att_suplement.png"), width = 10, height = 12)
# grid.arrange(grobs = plot.list, layout_matrix = lay)

for (i in 1:4) {
  # i = 1
  # i=3
  var_eng <- variables_eng[i]
  var_esp <- variables_esp[i]
  files <- list.files(dir, pattern = ".tif$", full.names = T);files
  files.var <- files[grep(pattern = var_eng, x = files)];files.var
  
  index <- lapply(depths.all, function(d){grep(pattern = d, files.var)}) %>% unlist;index
  imgs <- stack(files.var[index]);imgs
  var_title = c("A) Bulk Density", "B) Clay",  "C) Sand", "D) Silt")
  names(imgs) <- paste0(depths.all,"cm")
  if (i == 1){breaks = seq(0.1,2.3,0.22)}
  if (i == 2){breaks = seq(0,70,10)}
  if (i == 3){breaks = seq(0,100,10)}
  if (i == 4){breaks = seq(0,90,10)}
  colpal <- c("#001219","#005F73", "#0A9396", "#94D2BD", "#E9D8A6", "#EE9B00","#CA6702","#BB3E03","#ae2012","#9b2226")
  my.settings <- list(
    par.main.text = list(font = 2, # make it bold
                         just = "left", 
                         x = grid::unit(5, "mm")))
  
  lvlp <- rasterVis::levelplot(imgs, col.regions = colpal, at = breaks, main = var_title[i], par.settings=my.settings, names.attr = paste0(depths.all,"cm"));lvlp
  png(filename = here("results","figures", str_c(var_eng,"_depths_all.png")), width = 600, height = 800, units = "px")
  print(lvlp)
  dev.off()
  plot.list[[i]] <- lvlp
}

plot.list[[4]]
lay <- rbind(c(1,2),
             c(3,4))

lay <- rbind(c(1),
             c(2),
             c(3),
             c(4))
g <- arrangeGrob(grobs = plot.list, layout_matrix = lay)
ggsave(plot = g, filename = here("results","figures","CR2SOILS_physical_att_all.png"), width = 25, height = 35)
grid.arrange(grobs = plot.list, layout_matrix = lay)

######################## FIN PARTE 11 ####################
############# PARTE 12: PREDICTORES ######################

# Variables globales
depths.nam <- c("2.5" = "0-5 cm" ,"10" = "5-15 cm", "22.5" = "15-30 cm","45" = "30-60 cm","80" = "60-100 cm","150" = "100-200 cm")
depths <- c(2.5, 10, 22.5, 45, 80, 150)
var.eng <- c(Da = "Bulk Density", Arcilla = "Clay", Limo = "Silt", arena = "Sand")
variables <- c("Da","Arcilla","arena","Limo")

# tipos de covariables
cov.type <- read_csv(here("proc","var_sel","covariables.csv"))
cov.type
clase <- factor(cov.type[[2]]);clase
levels(clase)

# Definir escala de colores
myColors <- c("#0072B2", "#CC79A7","#9A8447", "#009E73")
names(myColors) <- levels(clase);myColors
colScale <- scale_fill_manual(name = NULL,values = myColors[-2], limits = levels(clase)[-2], drop = F)

# nombres arreglados de covariables seleccionadas
cov_names <- read_csv(here("proc","var_sel","selected_covs.csv"))

plot.list <- list()
for (i in 2:3) {
  # i = 1
  # seleccionar variable
  var <- variables[i]
  
  #leer variables predictoras upper y lower
  var_sel_l <- read_rds(here("proc","var_sel",str_c(var,"_lower_uncor.rds")))
  var_sel_u <- read_rds(here("proc","var_sel",str_c(var,"_upper_uncor.rds")))
  
  #Upper predictors
  predictors <- var_sel_u$pred_names;predictors
  importance <- var_sel_u$thres$imp.mean.dec[match(predictors, var_sel_u$imp_names)];importance
  imp_total <- sum(importance)
  relative.imp <- 100*importance/imp_total
  clase <- cov.type$tipo[match(predictors, cov.type[[1]])]
  
  data_u <- data.frame(predictors, relative.imp, clase, model = "upper");data_u
  
  #Lower predictors
  predictors <- var_sel_l$pred_names;predictors
  importance <- var_sel_l$thres$imp.mean.dec[match(predictors, var_sel_l$imp_names)]
  imp_total <- sum(importance)
  relative.imp <- 100*importance/imp_total
  clase <- cov.type$tipo[match(predictors, cov.type[[1]])]
  
  data_l <- data.frame(predictors, relative.imp, clase, model = "lower");data_l
  
  # unir dataframes con predictores upper y lower
  data <- bind_rows(data_l,data_u);data
  # cambiar el orden de factores para plotear
  data <- transform(data, model=factor(model, levels = c("upper","lower")))
  
  # cambiar nombre de las variables
  data <- data %>% mutate(predictors = cov_names$plot_name[match(data$predictors, cov_names$original)]);data
  limit <- as.integer(1+data$relative.imp %>% max)
  ##### Variable Importance #####
  
  # plot var imp scales free
  # ggplot(data = data, aes(x = reorder(predictors, relative.imp), y = relative.imp,
  #                         fill = clase)
  # ) +
  #   facet_wrap(. ~ model, 
  #              scales = "free",
  #              labeller = labeller(model = c("upper" = "Superficie: 0-30cm", "lower" = "Profundidad: 30-200cm")))+
  #   labs(x = "predictores", y = "Importancia rel. %") +
  #   geom_col() +
  #   coord_flip() +
  #   colScale +
  #   theme(legend.position = "bottom")
  
  setTitle <- function(var){
    if(var == "Da"){return("A) Bulk Density")}
    if(var == "Arcilla"){return("B) Clay")}
    if(var == "arena"){return("C) Sand")}
  }
  
  setLegend <- function(var){
    if(var == "Da"){return("none")}
    if(var == "Arcilla"){return("none")}
    if(var == "arena"){return("bottom")}
  }
  
  setJust <- function(var){
    if(var == "Da"){
      return(-1)
    }else{
        return(-0.6)
      }
  }
  
  # plot var imp - ambos ordenados de mayor a menor
  plot.upper <- data %>% filter(model == "upper") %>% mutate(title = "Upper Set: 0-30cm") %>% 
    ggplot(aes(x = reorder(predictors, relative.imp), y = relative.imp,
               fill = clase)
    ) +
    labs(y= "Rel. Importance %", x = "Predictors") +
    geom_col() +
    coord_flip() +
    colScale +
    ylim(0,limit)+
    facet_wrap(. ~ title)+
    theme(legend.position=setLegend(var), plot.title = element_text(hjust = setJust(var)))
  
  plot.lower <- data %>% filter(model == "lower") %>% mutate(title = "Lower Set: 30-200cm") %>% 
    ggplot(aes(x = reorder(predictors, relative.imp), y = relative.imp,
               fill = clase)
    ) +
    labs(y= "Rel. Importance %", x = "Predictors") +
    geom_col() +
    coord_flip() +
    colScale +
    ylim(0,limit) +
    facet_wrap(. ~ title)+
    theme(legend.position=setLegend(var), plot.title = element_text(hjust = -1))
  arrange <- ggpubr::ggarrange(plot.upper, plot.lower, nrow = 1, ncol = 2, common.legend = T, legend= setLegend(var))
  plot.list[[i]] <- arrange;arrange
  ggsave(here("results","figures",paste0("selected_covs_",var, ".png")), arrange, width = 20, height = 10, units = "cm")
  ggsave(here("results","figures",paste0("selected_covs_",var, ".eps")), arrange, width = 20, height = 10, units = "cm")
  
  }

arrange <- ggpubr::ggarrange(plotlist = plot.list, nrow = 3, ncol = 1, 
                             labels = list("A) Bulk Density","B) Clay","C) Sand"),
                             common.legend = TRUE, legend="bottom",
                             hjust = c(0.008,0,0));arrange
ggsave(here("results","figures","selected_covs.png"), arrange, width = 20, height = 20, units = "cm")
ggsave(here("results","figures","selected_covs.eps"), arrange, width = 20, height = 20, units = "cm")


######################## FIN PARTE 12 ####################
#####-----------------------------------------------######
############# PARTE 13: TABLA ESTADISTICA MAPAS POST-PROCESADOS ######################
# Variables globales
depths.nam <- c("2.5" = "0-5 cm" ,"10" = "5-15 cm", "22.5" = "15-30 cm","45" = "30-60 cm","80" = "60-100 cm","150" = "100-200 cm")
depths <- c(2.5, 10, 22.5, 45, 80, 150)
var.eng <- c(Da = "Bulk Density", Arcilla = "Clay", Limo = "Silt", arena = "Sand")
variables <- c("Da","Arcilla","arena","Limo")
outdir <- here("results","tables")

# mapas post-procesados
files<- list.files(here("results","postproc","v1","SoilMaps_MEAN"), full.names = TRUE, pattern = ".tif")
variables = c("Clay","Sand","Silt","Bulkd")
for (i in 1:4){
  var = variables[i]
  print(var)
  
  x.f <- files[grep(var, files)]
  img <- rast(x.f)
  img <- img[[c(1,5,3,4,6,2)]]
  val.img <- values(img)
  val.summary = val.img %>% as_tibble %>% drop_na %>% pivot_longer(cols = 1:6) %>% 
    group_by(name) %>% 
    summarise(mean = mean(value),
              median = median(value),
              sd = sd(value), 
              max = max(value),
              min = min(value),
              iqr = IQR(value),
              q0.25 = quantile(value, 0.25),
              q0.75 = quantile(value, 0.75)
    )
  val.summary %>% write_csv(here(outdir, str_c(var, "_density_summary.csv")))
}

######################## FIN PARTE 13 ####################
#####-----------------------------------------------######
############# PARTE 14: PLOTS UNCERTAINTY VS PREDICTED VALUES ######################
# Variables globales
depths.nam <- c("2.5" = "0-5cm" ,"10" = "5-15cm", "22.5" = "15-30cm","45" = "30-60cm","80" = "60-100cm","150" = "100-200cm")
depths <- c(2.5, 10, 22.5, 45, 80, 150)
variables <- c("Bulkd","Clay","Sand","Silt")
var.eng <- c("Bulk Density", "Clay", "Sand","Silt")

outdir <- here("results","figures")

p.list <- list()
for (i in 2:3){
  # i=1
  var <- variables[i]
  print(var)
  # mapas post-procesados PIRANGE
  files <- list.files(here("results","postproc","v1","PIRange"),
                           full.names = TRUE, pattern = ".tif");pir.files
  pir.files <- files[grep(var, files)][c(1,5,3,4,6,2)];pir.files
  # mapas post-procesados MEAN PREDICTION
  files<- list.files(here("results","postproc","v1","SoilMaps_MEAN"), full.names = TRUE, pattern = ".tif")
  mean.files <- files[grep(var, files)][c(1,5,3,4,6,2)];mean.files
  
  imgs <- c(pir.files, mean.files) %>% rast
  names(imgs)
  names(imgs)[1:6] <- paste0(var,".", depths.nam,"_PI")
  samp.df <- spatSample(imgs, size = 100000, method = "random", na.rm = TRUE, as.df = TRUE)
  samp.df
  
  pi <- samp.df %>% dplyr::select(ends_with("PI")) %>% 
    pivot_longer(cols = 1:6, names_to = "depth_pi",values_to = "PI") %>% 
    dplyr::select(PI)
  m <- samp.df %>% dplyr::select(-ends_with("PI")) %>% 
    pivot_longer(cols = 1:6, names_to = "depth",values_to = "mean")
  
  str_split(samp.df$depth[1:100], pattern = "\\.") %>% 
    sapply(FUN = function(x){x[[2]]}) %>% 
    factor(levels = c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm"))
  
  samp.df <- m %>% bind_cols(pi)
  # rm(m);rm(pi)
  samp.df <- samp.df %>% mutate(depth = str_split(depth, pattern = "\\.") %>%
                                  sapply(FUN = function(x){x[[2]]}) %>% 
                                  factor(levels = c("0-5cm","5-15cm","15-30cm","30-60cm","60-100cm","100-200cm"))
  )
  samp.df
  
  p <- ggplot(samp.df, aes(x = mean, y = PI))+
    geom_pointdensity(show.legend = FALSE, alpha = 0.5)+
    geom_smooth(method = "lm")+
    # ggpubr::stat_cor()+
    # scale_fill_gradient(low = "yellow" , high = "red", space = "Lab")+
    labs(x = if_else(var == "Bulkd", "Bulk density", var), y = "Prediction Interval width")+
    theme_bw()+
    scale_color_viridis()+
    theme(axis.title = element_text(size = 16))
  p.list[[i]] <- p
  ggsave(filename = here(outdir,str_c(var,"_PI_vs_mean_points_density.png")), width = 4, height = 4)
  ggsave(filename = here(outdir,str_c(var,"_PI_vs_mean_points_density.eps")), width = 4, height = 4)
  
}
######################## FIN PARTE 14 ####################
#####-----------------------------------------------######

############# PARTE 15: FC AND PWP VALIDATION ######################
# depths levels
depths <- c("0-5 cm", "5-15 cm", "15-30 cm", "30-60 cm", "60-100 cm", "100-200 cm")

# Load Dataframe with observed and simulated values
# Field Capacity
fname <- here("proc", "correlations", "FC_correlation.csv")
data <- read_csv(fname) %>%
  mutate(depth = factor(depth, levels = depths))

# calculate metrics
annotate_metrics <- function(sim, obs) {
  m <- hydroGOF::gof(sim, obs, norm = "maxmin")
  msg <- paste0("R2 = ", m["R2", ], "\nRMSE = ", m["RMSE", ], "\nNRMSE = ", m["NRMSE %", ], "%", "\nPBIAS = ", m["PBIAS %", ], "%")
  as.character(msg)
}
metrics <- data %>%
  group_by(depth) %>%
  do(data.frame(message = annotate_metrics(.$sim, .$obs)))
metrics

# set default theme
theme_set(theme_bw())
p1 <- ggplot(data, aes(x = obs, y = sim)) +
  geom_point(alpha = 0.7, size = 0.7) +
  geom_label(data = metrics, aes(label = message), x = 0.3, y = 0.75, size = 2) +
  geom_smooth(method = "lm", se = T) +
  geom_abline(slope = 1, intercept = 0) +
  tune::coord_obs_pred() +
  labs(x = "observed", y = "simulated", title = "A) Field Capacity") +
  facet_wrap(depth ~ .)
plot(p1)
ggsave(filename = here("results", "figures", "FC_correlation_R2.png"), width = 7, height = 5, plot = p1)
ggsave(filename = here("results", "figures", "FC_correlation_R2.eps"), width = 7, height = 5, plot = p1)

# Permanent Wilting Point
fname <- here("proc", "correlations", "PWP_correlation.csv")
data <- read_csv(fname) %>%
  mutate(depth = factor(depth, levels = depths))

metrics <- data %>%
  group_by(depth) %>%
  do(data.frame(message = annotate_metrics(.$sim, .$obs)))

# set default theme
theme_set(theme_bw())
p2 <- ggplot(data, aes(x = obs, y = sim)) +
  geom_point(alpha = 0.7, size = 0.7) +
  geom_label(data = metrics, aes(label = message), x = 0.15, y = 0.44, size = 2) +
  geom_smooth(method = "lm", se = T) +
  geom_abline(slope = 1, intercept = 0) +
  tune::coord_obs_pred() +
  labs(x = "observed", y = "simulated", title = "B) Permanent Wilting Point") +
  facet_wrap(depth ~ .)
plot(p2)
ggsave(filename = here("results", "figures", "PWP_correlation_R2.png"), width = 7, height = 5, plot = p2)
ggsave(filename = here("results", "figures", "PWP_correlation_R2.eps"), width = 7, height = 5, plot = p2)

arrange <- ggpubr::ggarrange(
  plotlist = list(p1, p2), nrow = 2, ncol = 1,
  hjust = c(0.008, 0, 0)
)

arrange
ggsave(filename = here("results", "figures", "PWPandFC_correlation.png"), width = 7, height = 9)
ggsave(filename = here("results", "figures", "PWPandFC_correlation.eps"), width = 12, height = 6)

######################## FIN PARTE 15 ####################
#####-----------------------------------------------######





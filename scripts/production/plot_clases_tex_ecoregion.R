library(sf)
library(tidyverse)
library(terra)
library(gridExtra)
library(soiltexture)

files <- list.files("D:/CLSoilMaps/results/postproc/v1/Textural_Classes", full.names = TRUE, pattern = ".tif$");files
r = rast(files)

#outdir
outdir = here::here("results","figures")

class_levels = paste0("clase_", c(1,2,4,3,5,6,7,8,9,10,11));class_levels
class_color = c("#dc0f0f","#44ce5d","#de8313","#7533e6","#455dca","#98e16e","#bb3cc9","#dfef4d","#3feabd","#cf3c8d","#067b8a")
class_names = c("Cl","ClLo","Lo","LoSa","Sa","SaCl","SaClLo","SaLo","SiCl","SiCiLo","SiLo")

names(r)
r_i = r[[1]]
r_i

class_n = r_i == 1

# aggregate the columns
a <- aggregate(class_n, c(1, ncol(r_i)), sum, na.rm = TRUE)

# get the latitude for each row
lat <- yFromRow(a, 1:nrow(a))

# init df
df = tibble(clase_1 = values(a));df

#loop
# 11 clases
for (i in 2:11) {
  print(i)
  class_n = r_i == i
  a <- aggregate(class_n, c(1, ncol(r_i)), sum, na.rm = TRUE)
  
  df <- bind_cols(df, values(a));df
  names(df)[ncol(df)] <- paste0("clase_",i)
}
df = bind_cols(lat, df) %>% drop_na()
names(df)[c(1,2)] <- c("latitude","clase_1")
names(df)
df

ggplot(df)+
  geom_col(aes(x = latitude, y = clase_3))+
  coord_flip()

df

df.tidy = df %>% pivot_longer(cols = clase_1:clase_11, names_to = "clase", values_to = "value") %>% 
  mutate(clase = factor(clase, levels = class_levels),
         lat_2= round(latitude, 0)) %>% 
  group_by(lat_2, clase) %>% 
  summarise(sum_val = sum(value)) %>% 
  ungroup() %>% 
  group_by(lat_2) %>% 
  mutate(lat_total = sum(sum_val),
         ratio = 100*sum_val/lat_total)
df.tidy

# df.tidy %>% 
#   ggplot()+
#   geom_col(aes(x = lat_2, y = mean_val, fill = clase), position = "fill")+
#   scale_fill_manual(values = class_color, labels = class_names)+
#   coord_flip()

area.plot = df.tidy %>% 
  ggplot()+
  geom_area(aes(x = lat_2, y = ratio, fill = clase), position = "fill")+
  scale_fill_manual(values = class_color, labels = class_names)+
  coord_flip()+
  labs(y = "cover ratio", x = "latitude", fill = "Texture Class","proportion of clases by latitude")
area.plot
ggsave(plot = area.plot, filename = here::here("results","figures","tex_dist_by_lat.pdf"), height = 6, width = 7)

# Analisis por ecoregion --------------------------------------------------

# ecoregiones
eco = read_sf("D:/CLSoilMaps/data/shp/ecoregiones/ecoregions_chile_camels_dissolved.shp") %>%
  st_transform(4326)


ggplot(eco)+
  geom_sf(aes(fill = ECO_NAME),color = NA)+
  # scale_fill_manual(values = class_color, labels = class_names)+
  labs(x = "longitude", y = "latitude", fill = "EcoRegions")

# match(tex_eco$ID, eco_id$ID) %>% length
# match(tex_eco$ID, eco_id$ID) %>% unique
# tex_eco %>% nrow

tex_eco = extract(r_i, eco)
# tex_eco$`Tex_Class.0-5cm` %>% unique
eco_id = tibble(eco_name = eco$ECO_NAME, ID = 1:8)
tex_eco$ECO_NAME = eco_id$eco_name[match(tex_eco$ID,eco_id$ID)]
tex_eco = tex_eco %>% filter(ID != 8)
df = tibble()
for (i in 1:7) {
  # i=1
  cat(i, " ")
  n = eco_id$eco_name[i]
  f = tex_eco %>% filter(ECO_NAME == n) %>% select(2) %>% as.vector %>% table
  f
  t = tibble(tex_class = class_names[as.numeric(names(f))], count = as.numeric(f), ECO_NAME = n)
  t
  df = bind_rows(df, t)
}
df


for (i in 1:7) {
  n = eco_id$eco_name[i]
  p = ggplot(df %>% filter(ECO_NAME == n))+
    geom_col(aes(x = tex_class, y = count, fill = tex_class), show.legend = FALSE)+
    coord_polar()+
    labs(title = n, fill = "Textural Class")+
    scale_fill_manual(values = class_color, breaks = class_names)
  plot(p)
}

# eco_levels <- names(c("Atacama desert"="Atacama Desert",
#   "Central Andean dry puna" ="C. Andean. Dry Puna",
#   "Southern Andean steppe" = "S. Andean Steppe",
#   "Chilean Matorral"= "Chilean Matorral",
#   "Valdivian temperate forests" = "Vald. Temp. Forest",
#   "Magellanic subpolar forests" = "Mag. Subpolar Forest",
#   "Patagonian steppe"= "Patagonian Steppe"));eco_levels

eco_levels <- c("Atacama desert"="AD",
                      "Central Andean dry puna" ="CADP",
                      "Southern Andean steppe" = "SAS",
                      "Chilean Matorral"= "CM",
                      "Valdivian temperate forests" = "VTF",
                      "Magellanic subpolar forests" = "MSF",
                      "Patagonian steppe"= "PS");eco_levels
# eco_levels <- c("AD"="Atacama Desert",
#                       "CADP" = "Central Andean dry puna",
#                       "SAS" = "Southern Andean steppe",
#                       "CM"= "Chilean Matorral",
#                       "VTF" = "Valdivian temperate forests",
#                       "MSF" = "Magellanic subpolar forests",
#                       "PS" = "Patagonian steppe");eco_levels

porc = df %>% group_by(ECO_NAME) %>% 
  summarise(total_count = sum(count)) %>% ungroup

# full_join(df, porc) %>% 
#   mutate(porcentaje = round(100*count/total_count,1)) %>% 
#   mutate(ECO_NAME = factor(ECO_NAME, levels = eco_levels)) %>% 
#   ggplot(aes(x = "", y = porcentaje, fill = tex_class))+
#   geom_col()+
#   facet_wrap(.~ECO_NAME, drop = FALSE, ncol = 2)+
#   scale_fill_manual(values = class_color, breaks = class_names)+
#   labs(x = "", y = "")
#   # coord_polar(theta = "y")
# ggsave(paste0(outdir, "/Porcentaje de clases por ecoregion_barplot.png"), height = 8, width = 6)

full_join(df, porc) %>% 
  mutate(porcentaje = round(100*count/total_count,1)) %>% 
  mutate(ECO_NAME = factor(ECO_NAME, levels = names(eco_levels))) %>% 
  ggplot(aes(x = "", y = porcentaje, fill = reorder(tex_class, porcentaje)))+
  geom_col(show.legend = FALSE)+
  facet_wrap(.~ECO_NAME, drop = FALSE, ncol = 2, labeller = as_labeller(eco_levels))+
  scale_fill_manual(values = class_color, breaks = class_names)+
  labs(x = "percentage of cover", y = "")+
  coord_polar(theta = "y")

# "Cl"     "ClLo"   "LoSa"   "Lo"     "Sa"     "SaCl" 
# "SaClLo" "SaLo"   "SiCl"   "SiCiLo" "SiLo"
# "#dc0f0f" "#44ce5d" "#de8313" "#7533e6" "#455dca" "#98e16e" 
# "#bb3cc9" "#dfef4d" "#3feabd""#cf3c8d" "#64caef"

ggsave(paste0(outdir, "/Porcentaje de clases por ecoregion_piechart_0_5cm.png"), height = 8, width = 6)
ggsave(paste0(outdir, "/Porcentaje de clases por ecoregion_piechart_0_5cm.pdf"), height = 8, width = 6)
ggsave(paste0(outdir, "/Porcentaje de clases por ecoregion_piechart_0_5cm.svg"), height = 8, width = 6)

full_join(df, porc) %>% 
  mutate(porcentaje = round(100*count/total_count,1)) %>% 
  mutate(ECO_NAME = factor(ECO_NAME, levels =names(eco_levels))) %>% 
  write_csv("./results/tables/Tabla_porcentaje_clases_texturales_por_ecoregion.csv")

for (i in 1:7) {
  n = eco_id$eco_name[i]
  print(n)
  data = full_join(df, porc) %>% 
    mutate(porcentaje = round(100*count/total_count,1)) %>% 
    mutate(ECO_NAME = factor(ECO_NAME, levels = names(eco_levels)))
  p = ggplot(data %>% filter(ECO_NAME == n), aes(x = "", y = porcentaje, fill = tex_class))+
    geom_col()+
    scale_fill_manual(values = class_color, breaks = class_names)+
    coord_polar(theta = "y")
  plot(p)
}



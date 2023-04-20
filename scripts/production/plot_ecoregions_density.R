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
                "conflicted",
                "lubridate") # list of packages to load
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
  
  # CONFLICTS -----------------------------------------
  conflicted::conflict_prefer("select", "dplyr")
  conflicted::conflict_prefer("filter", "dplyr")
  conflicted::conflict_prefer("extract", "terra")
  conflicted::conflict_prefer("rename","dplyr")

  # LOAD FUNCTIONS ------------------------------------
  # space reserved for your functions
}

# ecoregiones
eco = read_sf("D:/CLSoilMaps/data/shp/ecoregiones/ecoregions_chile_camels_dissolved.shp") %>%
  st_transform(4326) %>%
  filter(ECO_NAME != "Rock and Ice")

# variables a extraer
variables = c("Bulkd", "Clay","Sand","Silt")

dataset = read.csv(here("proc","tables","extr_stand_soilprof_paper.csv"))

dataset %>% ggplot()+
  geom_density(aes(x = value_sim, color = "CLSoilMaps"))+
  geom_density(aes(x = value_sg, color = "SoilGrids"))+
  geom_density(aes(x = value_obs, color = "Observed"))+
  facet_wrap(var~eco_name, scales = "free", dir = "v", nrow=7)

dataset %>% ggplot()+
  geom_point(aes(x = value_obs, y = value_sim))+
  facet_wrap(var~eco_name, scales = "free")

okabe <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


dataset %>%
  pull(eco_name) %>% 
  unique()
eco_names = c("Atacama desert",
              "Central Andean dry puna",
              "Southern Andean steppe",
              "Chilean Matorral",
              "Valdivian temperate forests",
              "Patagonian steppe", 
              "Magellanic subpolar forests")

dataset = dataset %>% 
  mutate(eco_name = factor(eco_name, levels = eco_names))


theme_bw()
for (i in 1:4) {
  # i=1
  if (variables[i] == "Bulkd"){
    p = dataset %>% filter(var == variables[i]) %>% 
      ggplot()+
      geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_sg, color = "SoilGrids"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_obs, color = "Observed"), 
                   linewidth = 0.8,
                   linetype = "dashed")+
      facet_wrap(.~eco_name, scales = "free",ncol = 1)+
      scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
                         labels = c("CLSoilMaps","Oberved","SoilGrids"))+
      labs(x = "", color = "", title = "Bulk Density")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            strip.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.position = "bottom",
            legend.text = element_text(size = 17.5))
  }else if(variables[i] == "Silt"){
    p = dataset %>% filter(var == variables[i]) %>% 
      ggplot()+
      geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_sg, color = "SoilGrids"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_obs, color = "Observed"), 
                   linewidth = 0.8,
                   linetype = "dashed")+
      facet_wrap(.~eco_name, scales = "free",ncol = 1)+
      scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
                         labels = c("CLSoilMaps","Oberved","SoilGrids"))+
      labs(x = "", color = "", title = "Silt")+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            strip.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.position = "bottom",
            legend.text = element_text(size = 17.5))
  }else{
    p = dataset %>% filter(var == variables[i]) %>% 
      ggplot()+
      geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_sg, color = "SoilGrids"), 
                   linewidth = 0.8)+
      geom_density(aes(x = value_obs, color = "Observed"), 
                   linewidth = 0.8,
                   linetype = "dashed")+
      facet_wrap(.~eco_name, scales = "free",ncol = 1)+
      scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
                         labels = c("CLSoilMaps","Oberved","SoilGrids"))+
      labs(x = "", color = "", title = variables[i])+
      theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
            strip.text = element_text(size = 20),
            axis.title = element_text(size = 20),
            legend.position = "bottom",
            legend.text = element_text(size = 17.5))
  }
  plot(p)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".png")), height = 17, width = 5)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".pdf")), height = 17, width = 5)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".svg")), height = 17, width = 5)
}

facet_wrap()
theme_bw()
plot(eco)
eco$ECO_NAME = factor(eco$ECO_NAME, levels = eco_names)
ggplot(eco)+
  geom_sf(aes(fill = ECO_NAME), color = NA)+
  labs(fill = "Ecoregion")+
  scale_fill_manual(values = okabe)
  # scico::scale_fill_scico_d(palette = "batlow")
ggsave(here("results","figures",paste0("ecoregions.png")), height = 11, width = 4)
ggsave(here("results","figures",paste0("ecoregions.pdf")), height = 11, width = 4)
ggsave(here("results","figures",paste0("ecoregions.svg")), height = 11, width = 4)


# standardized soilprofiles
files = list.files(here("D:/CLSoilMaps/proc/soil_database/standard"), full.names = TRUE);files

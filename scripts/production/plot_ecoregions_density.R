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
eco_names_ab = c("AD",
              "CADP",
              "SAS",
              "CM",
              "VTF",
              "PS", 
              "MSF")
names(eco_names_ab) = eco_names

dataset = dataset %>% 
  mutate(eco_name = factor(eco_name, levels = eco_names),
         eco_name_ab = factor(eco_names_ab[eco_name],levels = eco_names_ab))
dataset
count_data = count(drop_na(dataset), eco_name_ab, var)
count_data

dataset %>% 
  group_by(var, eco_name_ab) %>% 
  summarise(sim = sum(!is.na(value_sim))) %>% 
  print(n = 28)


names(variables) = c("Bulk Density","Clay","Sand","Silt")
theme_set(theme_bw())
for (i in 1:4) {
  # i=1
  {
    # if (variables[i] == "Bulkd"){
    #   p = dataset %>% filter(var == variables[i]) %>% 
    #     ggplot()+
    #     geom_density(aes(x = value_obs, color = "Observed"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sg, color = "SoilGrids"), 
    #                  linewidth = 0.8)+
    #     geom_text(data = count_data %>% filter(var == variables[i]),
    #               aes(x = Inf, y = Inf, label = str_c("N = ",n)),
    #               hjust = 1.2, vjust = 1.2)+
    #     facet_wrap(.~eco_name_ab, scales = "free",ncol = 1)+
    #     scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
    #                        labels = c("CLSoilMaps","Observed","SoilGrids"))+
    #     labs(x = "", color = "", title = "Bulk Density")+
    #     theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    #           strip.text = element_text(size = 20),
    #           axis.title = element_text(size = 20),
    #           legend.position = "bottom",
    #           legend.text = element_text(size = 17.5))
    # }else if(variables[i] == "Silt"){
    #   p = dataset %>% filter(var == variables[i]) %>% 
    #     ggplot()+
    #     geom_density(aes(x = value_obs, color = "Observed"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sg, color = "SoilGrids"), 
    #                  linewidth = 0.8)+
    #     geom_text(data = count_data %>% filter(var == variables[i]),
    #               aes(x = Inf, y = Inf, label = str_c("N = ",n)),
    #               hjust = 1.2, vjust = 1.2, size = 20)+
    #     facet_wrap(.~eco_name_ab, scales = "free",ncol = 1)+
    #     scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
    #                        labels = c("CLSoilMaps","Observed","SoilGrids"))+
    #     labs(x = "", color = "", title = "Silt")+
    #     theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    #           strip.text = element_text(size = 20),
    #           axis.title = element_text(size = 20),
    #           axis.text = element_text(size = 18),
    #           legend.position = "bottom",
    #           legend.text = element_text(size = 17.5))
    # }else{
    #   p = dataset %>% filter(var == variables[i]) %>% 
    #     ggplot()+
    #     geom_density(aes(x = value_obs, color = "Observed"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
    #                  linewidth = 0.8)+
    #     geom_density(aes(x = value_sg, color = "SoilGrids"), 
    #                  linewidth = 0.8)+
    #     geom_text(data = count_data %>% filter(var == variables[i]),
    #               aes(x = Inf, y = Inf, label = str_c("N = ",n)),
    #               hjust = 1.2, vjust = 1.2)+
    #     facet_wrap(.~eco_name_ab, scales = "free",ncol = 1)+
    #     scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
    #                        labels = c("CLSoilMaps","Observed","SoilGrids"))+
    #     labs(x = "", color = "", title = variables[i])+
    #     theme(plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
    #           strip.text = element_text(size = 20),
    #           axis.title = element_text(size = 20),
    #           legend.position = "bottom",
    #           legend.text = element_text(size = 17.5))
    #   
    #   
    # }
  }
  
  p = dataset %>% filter(var == variables[i]) %>% 
    ggplot()+
    geom_density(aes(x = value_obs, color = "Observed"), 
                 linewidth = 0.8, linetype = "dashed")+
    geom_density(aes(x = value_sim, color = "CLSoilMaps"), 
                 linewidth = 0.8)+
    geom_density(aes(x = value_sg, color = "SoilGrids"), 
                 linewidth = 0.8)+
    geom_text(data = count_data %>% filter(var == variables[i]),
              aes(x = Inf, y = Inf, label = str_c("N = ",n)),
              hjust = 1.2, vjust = 1.2, size = 6)+
    facet_wrap(.~eco_name_ab, scales = "free",ncol = 1)+
    scale_color_manual(values = c( "#D55E00","#000000","#0072B2"),
                       labels = c("CLSoilMaps","Observed","SoilGrids"))+
    labs(x = "", color = "", title = names(variables)[i])+
    theme(plot.title = element_text(hjust = 0.5, size = 28, face = "bold"),
          strip.text = element_text(size = 18),
          axis.title = element_blank(),
          axis.text = element_text(size = 13),
          legend.position = "bottom",
          legend.text = element_text(size = 24))
  # plot(p)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".png")), height = 12, width = 4)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".pdf")), height = 12, width = 4)
  ggsave(plot = p, here("results","figures",paste0("ecoregion_distribution_",variables[i], ".svg")), height = 12, width = 4)
}

plot(eco)
eco$ECO_NAME = factor(eco$ECO_NAME, levels = eco_names)
ggplot(eco)+
  geom_sf(aes(fill = ECO_NAME), color = NA)+
  labs(fill = "Ecoregion")+
  scale_fill_manual(values = okabe)
  # scico::scale_fill_scico_d(palette = "batlow")
ggsave(here("results","figures",paste0("ecoregions.png")), height = 10, width = 3)
ggsave(here("results","figures",paste0("ecoregions.pdf")), height = 10, width = 3)
ggsave(here("results","figures",paste0("ecoregions.svg")), height = 10, width = 3)


# standardized soilprofiles
files = list.files(here("D:/CLSoilMaps/proc/soil_database/standard"), full.names = TRUE);files

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
                "conflicted") # list of packages to load
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


# Load maps of field capacity and permanent wilting point -----------------
files = dir(here("results","postproc","v1","ROSETTA_MEAN"), full.names = TRUE)
fc.files = files[grep("FC",files)]
pwp.files = files[grep("PWP",files)]

fc = rast(fc.files[c(1,5,3,4,6,2)])
pwp = rast(pwp.files[c(1,5,3,4,6,2)])

# Load soil database ------------------------------------------------------
fname = here("proc","soil_database","standard","FC_standard.csv")
soil_db = read_csv(fname) %>% filter(id != 2108)
depths = names(soil_db)[2:7]

scico_palette_show()
# DATABASE 
x = soil_db %>% pivot_longer(cols = 2:7, names_to = "depth") %>% 
  mutate(depth = factor(depth, levels = depths))
ggplot(x)+
  geom_density(aes(x = value, color = depth), size = 1.2)+
  scale_color_scico_d(palette = "roma")

ggplot(x)+
  geom_boxplot(aes(x = value, y = rev(depth)), size = 1)

filter(x, value > 0.75)

# Extract FC values
ext_val = lapply(1:6, function(i){
  # i=1
  d = depths[i]
  points = soil_db[,c("id", d, "x", "y")] %>% 
    st_as_sf(crs = 4326, coords = c("x","y"))
  v = extract(fc, vect(points))
  cat("matching soil db depth ",d, " with map ", names(v)[i+1], "\n")
  tibble(id = points$id, obs = points[[d]], sim = v[[i+1]], depth = d)
}) %>% 
  rlist::list.rbind()

write_csv(ext_val, here("proc","correlations","FC_correlation.csv"))

# Extract PWP values

fname = here("proc","soil_database","standard","PWP_standard.csv")
soil_db = read_csv(fname) %>% filter(id != 2108)
depths = names(soil_db)[2:7]

scico_palette_show()
# DATABASE 
x = soil_db %>% pivot_longer(cols = 2:7, names_to = "depth") %>% 
  mutate(depth = factor(depth, levels = depths))

ext_val = lapply(1:6, function(i){
  # i=1
  d = depths[i]
  points = soil_db[,c("id", d, "x", "y")] %>% 
    st_as_sf(crs = 4326, coords = c("x","y"))
  v = extract(pwp, vect(points))
  cat("matching soil db depth ",d, " with map ", names(v)[i+1], "\n")
  tibble(id = points$id, obs = points[[d]], sim = v[[i+1]], depth = d)
}) %>% 
  rlist::list.rbind()

write_csv(ext_val, here("proc","correlations","PWP_correlation.csv"))

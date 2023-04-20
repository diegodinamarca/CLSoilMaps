library(tidyverse)

bd = read_csv("D:/CLSoilMaps/data/soil_database/soil_database_paper.csv")
bd

bd[bd==-999] = NA
tibble(bd) %>% print(n = 100)


bd %>% select(ID, clay:VDMA) %>% 
  !(is.na(.)) %>% 
  colSums()

count = colSums(!(is.na(bd)))

df1 = tibble(prop = names(count), n = count) %>% slice(12:33)

un.prof = bd %>% select(ID, clay:VDMA) %>% 
  group_by(ID) %>% 
  slice(1)

count = colSums(!(is.na(un.prof)))

df2 = tibble(prop = names(count), n = count) %>% slice(2:23)
df2

full_join(df1, df2, by = "prop") %>% 
  rename(n_total = 2, n_prof = 3) %>% 
  write_csv(here::here("results","tables","resumen_bd.csv"))

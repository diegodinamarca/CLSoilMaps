# Packages
library(ithir)
library(aqp)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

# RMSE function
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}


# Load database
bdcsv <- read_csv("./materiales/Database/BD_soilprof_23MAR.csv")
names(bdcsv)

da <- bdcsv[,c("ID","x","y","top","bottom","Da","Tratamiento","Fuente")]
A <- bdcsv[,c("ID","x","y","top","bottom","Arcilla","Tratamiento","Fuente")]
L <- bdcsv[,c("ID","x","y","top","bottom","Limo","Tratamiento","Fuente")]
a <- bdcsv[,c("ID","x","y","top","bottom","arena","Tratamiento","Fuente")]
MO <- bdcsv[,c("ID","x","y","top","bottom","MO","Tratamiento","Fuente")]

bd.list <- list(da,A,a,MO,L)

# Constants
variables <- c("Da","Arcilla","arena","MO","Limo")
var_names_eng <- c( "Da" ="Bulk Density","Arcilla"= "Clay","arena" = "Sand","Limo" = "Silt", "MO"= "Organic Matter")
var_names_esp <- c( "Da" = "Da","A"= "Arcilla","a" = "arena","L" = "Limo", "MO" = "MO")
depths <- c('0-5 cm','5-15 cm','15-30 cm', "30-60 cm","60-100 cm","100-200 cm")

for (i in 1:5){
  #iterate through databases
  bd <- bd.list[[i]]
  var <- variables[i]
  
  nro_Reg <- nrow(bd);nro_Reg
  # Clean database
  bd <- bd[which(bd[[6]] != -999), ]
  bd <- bd[which(bd$Tratamiento == "testigo"), ] 

  #Define SoilProfile database
  data <- bd
  depths(data) <- ID ~ top + bottom
  
  #smoothing parameter
  lambda <- c(10, 1, 0.1, 0.01, 0.001,0.0001,0.00001)
  
  #GET RMSE for each lambda
  rmse_models <- sapply(lambda, function(x){
    tryCatch({
      # x <- 10
      eaFit <- ea_spline(testbd, var.name = var, #CAMBIAR COLUMNA SEGUN PROPIEDAD
                       d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = x, vlow = 0, vhigh = 100,
                       show.progress = TRUE)
      # plot(eaFIT, plot.which = 100)
      obs <- as.numeric(eaFit$obs.preds[[4]]) #CAMBIAR SEGUN PROPIEDAD
      pred <- eaFit$obs.preds$predicted
      rmse <- RMSE(pred, obs)
      res <- rmse},
      
             error = function(w){
               print(paste0("Error fitting splines for lambda = ", x))
               res <- 9999
             })
    
  })
  #Para el modelo de Da, A, L y a, el mejor RMSE fue del modelo con lambda 0.00001
  print(rmse_models)
  
  #Extract lambda with lowest RMSE
  lambda <- lambda[which.min(rmse_models)] 
  cat(var_names_eng[var], ": Selected Model lambda = ", lambda,"\n")
  
  # Get final results
  if (var == "Da"){
    eaFit <- ea_spline(testbd, var.name = var,
                       d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = lambda, vlow = 0,
                       show.progress = F)
  }else {
    eaFit <- ea_spline(testbd, var.name = var,
                       d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = lambda, vlow = 0, vhigh = 100,
                       show.progress = F)
  }
  
  # Export Harmonized database
  harm <- eaFit$harmonised
  index <- match(harm$id, bd$ID)
  harm$x <- bd$x[index]
  harm$y <- bd$y[index]
  harm$Fuente <- bd$Fuente[index]
  harm[harm == -9999] <- NA
  
  #Write result
  write_csv(harm, paste0("./materiales/Database/estandarizadas/",var,"_estandarizada.csv"))
  
  
  # Distribution of harmonized data
  plot <- harm %>% pivot_longer( cols = 2:7, names_to = "profundidad", values_to = "values") %>% 
    # PLOT
    ggplot() +
    geom_histogram(aes(x = values, fill = profundidad), color = "white") +
    # ordenar leyenda
    scale_fill_manual(breaks = depths, values = brewer.pal(6, "Dark2")) +
    labs(x = var_names_eng[var], title = paste0(var_names_eng[var], " Distribution per depth"))
  ggsave(plot, file = paste0("./Observed_", var_names_esp[var],"_distr.png"), height = 10, width = 14, units = "cm")
  
}

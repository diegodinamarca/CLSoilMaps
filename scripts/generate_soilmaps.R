###################################################################################################################
#################################### PREDICTIVE MAPS ##############################################################
###################################################################################################################

#Library
library(raster)
library(rgdal)
library(hydroGOF)
library(caret)
library(matrixStats)
library(doParallel)
library(SpatialML)
library(ranger)
library(tidyverse)
library(furrr)

#----- Functions ------------------- 
pred.bootmap <- function(model.path, data, method, outdir){
  library(raster, quietly = T) 
  library(ranger, quietly = T)
  nam <- str_split(model.path, pattern = "\\.") %>% unlist %>% .[3]
  rf <- readRDS(model.path)
  out <- str_c(outdir,"/",nam,".tif");out
  if (method == "RF"){
    pred.map <- predict(data, rf, verbose = F)
    writeRaster(pred.map, filename = out, overwrite = T)
  }else if (method == "QRF"){
    # ou36 V M,t <- str_c(outdir,"/",nam,"_q0.95.tif");out
    # pred.map <- predict(data, rf, type="quantiles", quantiles=c(0.95))  RT Y90,'¡
    # writeRaster(pred.map, filename = out, overwrite = T)

    out <- str_c(outdir,"/",nam,"_q0.5.tif");out
    pred.map <- predict(data, rf, type="quantiles", quantiles=c(0.5))
    writeRaster(pred.map, filename = out, overwrite = T)

    # out <- str_c(outdir,"/",nam,"_q0.05.tif");out
    # pred.map <- predict(data, rf, type="quantiles", quantiles=c(0.05))
    # writeRaster(pred.map, filename = out, overwrite = T)
  }
}
#--------Setup covariates---------------------------

# Model to predict, options: RF, GRF, QRF (Only RF available as of 22FEB 2023)
model <- "RF"
variables <- c("Da","Arcilla","arena")
depths <- c(2.5, 10, 22.5, 45, 80, 150)

# vector with best set of predictos selected for each depth
selection_clay <- paste0(c("upper","upper","lower","lower","lower","upper"),"_uncor")
selection_sand <- paste0(c("upper","upper","upper","upper","upper","upper"),"_uncor")
selection_bulkd <- paste0(c("upper","upper","upper","upper","lower","lower"),"_uncor")

# available cores
ncores <- detectCores()-1
set.seed(1234)
for (v in 2:2){
  var <- variables[v]
  for (s in c(3)){
    if (var == "Arcilla"){
      sel <- selection_clay[s]
    }
    if (var == "arena"){
      sel <- selection_sand[s]
    }
    if (var == "Da"){
      sel <- selection_bulkd[s]
    }

    # Model directory
    model.dir <- paste0("./resultados/modelos/",model,"/",var,"/");model.dir
    dir.create(model.dir, showWarnings = T)

    # Results Directory
    res.dir <- paste0("./resultados/mapas_pred/",model,"/",var,"/");res.dir
    dir.create(res.dir, showWarnings = T)

    # load model
    model.list <- list.files(path = str_c(model.dir,var,"_",sel), pattern = "\\.rds$", full.names = T);model.list %>% last
    rfmodel <- model.list[1] %>% read_rds

    # get vector of predictors
    cov.names <- rfmodel$forest$independent.variable.names
    print(cov.names)
    
    # exportar variables seleccionadas como csv
    data.frame(covariable = cov.names) %>% write_csv(file = paste0("./resultados/var_select/",var,"_",sel,"_selected.csv"))
    print(paste0("Iniciando proceso para: ", var))

    # create stack of predictors
    cargar_covs <- function(depth){
      cat("Cargando covariables ambientales... \n")
      covs <- c()

      #topografy
      f <- list.files(path = "./materiales/topografia/derivadas_topo",pattern = "tif$",full.names = T);f

      print("topografia")
      der_topo <- stack(f)

      #Climate
      f <- list.files(path = "./materiales/Climate", pattern = ".tif$",full.names = T);f
      print("clima")

      climate <- stack(f)

      #Geology
      f <- list.files(path = "./materiales/Geologia/rast", pattern = ".tif$", full.names = T);f
      print("geologia")

      geo <- stack(f)
      
      #spectral indexes
      f <- list.files(path = "./materiales/Landsat/indices",pattern = ".tif",full.names = T);f
      print("indices espectrales")
      
      indices <- stack(f)

      #Textures 11x11
      f <- list.files(path = "./materiales/Landsat/indices/GLCM11x11",pattern = "11x11",full.names = T);f
      print("11x11")

      texturas11x11 <- stack(f)

      #Textures 21x21
      f <- list.files(path = "./materiales/Landsat/indices/GLCM21x21",pattern = "21x21",full.names = T);f
      print("21x21")

      texturas21x21 <- stack(f)

      #Textures 51x51
      f <- list.files(path = "./materiales/Landsat/indices/GLCM51x51",pattern = "51x51",full.names = T);f
      print("51x51")

      texturas51x51 <- stack(f)
      
      # HydroSoilmap
      f <- list.files(path = "./materiales/Global_hydro_soil_map",pattern = ".tif$",full.names = T);f
      print("hydrosoilmap")
      hydrosoil <- stack(f)

      print("profundidad")

      depth.r <- raster(str_c("./materiales/depths/depth_",depth,".tif"))
      names(depth.r) = "depth"
      
      return(stack(der_topo, climate, geo, indices, texturas11x11, texturas21x21, texturas51x51, hydrosoil, depth.r))
    }
    
    covars <- cargar_covs(depths[s])
    names(covars)
    covars <- lapply(cov.names, function(p){
      covars[[grep(pattern = paste0("^",p,"$"), names(covars))]]
    }) %>% stack;covars
    
    
    if (!((length(cov.names)) == nlayers(covars))){
      cat("Problem matching covariates with their files \n")
    }

    # load stack of covariates
    pred.var <- stack(covars)
    names(pred.var) <- cov.names;names(pred.var)

    res.dir.depth <- str_c(res.dir,"depth_",depths[s],"_",sel);res.dir.depth
    dir.create(res.dir.depth, showWarnings = T)

    # bootstrap models
    bootstrap.files <- list.files(res.dir.depth)
    
    # index for removing existing bootstrap maps when previous execution failed for whatever reason
    index_rf <- str_split(bootstrap.files, "Bootstrap") %>% 
      unlist %>% str_split(".tif") %>% 
      unlist %>% as.integer %>%
      .[!is.na(.)];index_rf
    index_qrf <- str_split(bootstrap.files, "Bootstrap") %>%
      unlist %>% str_split("_") %>% 
      unlist %>% as.integer %>% 
      .[!is.na(.)] %>% unique;index_qrf
    
    # generate maps in parallel
    ncores <- 15
    cat("Starting Map Prediction... \n")
    set.seed(1234)
    plan(multisession, workers= ncores)
    if (length(index_rf) != 0){
      system.time(
        future_map(model.list[-index_rf], pred.bootmap, data = pred.var, method = model, outdir = res.dir.depth, .options = furrr_options(seed = T))
      )
    }else{
      system.time(
        future_map(model.list, pred.bootmap, data = pred.var, method = model, outdir = res.dir.depth, .options = furrr_options(seed = T))
      )
    }


    ## -------------calculate prediction MEAN, VAR, SD maps----------------------
    plan(sequential) #Cerrar los cluster y liberar memoria
    cat("Predicting statistics", var, "_",sel,"\n")
    # res.dir <- paste0("./resultados/mapas_pred/",model,"/",var,"/");res.dir
    # res.dir.depth <- str_c(res.dir,"depth_",depths[s],"_",sel);res.dir.depth
    var_name <- str_c(var, "X",depths[s],"_",sel);var_name


    # calculate average map
    if (model == "RF"){
      list.files(path = res.dir.depth, pattern = ".tif", full.names = TRUE)
      results <- stack(list.files(path = res.dir.depth, pattern = ".tif", full.names = TRUE));results
      avgmap <- mean(results, na.rm = T)
      names(avgmap) <- paste0("Mean_",var_name)
      writeRaster(avgmap, filename = paste0(res.dir,var_name,"_mean.tif"), overwrite = T)
      
      ## calculate variance for nrit iterations
      #variance <- sum( ( y - yprom )^2 ) / ( 1-nlayers )
      # from raster stack
      sqrdif <- (results-avgmap)^2
      n <- nlayers(results)
      var.res <-  sum(sqrdif)/(n-1)

      # export variance raster
      writeRaster(var.res, filename = paste0(res.dir,var_name,"_variance.tif"), overwrite = T)

      # overall variance of predictions
      list.files("./resultados/modelos", pattern = "RF", full.names = T)
      results <- read_rds(str_c("./resultados/modelos/",model,"_",var,"_",sel,"_results.rds"))
      names(results) <- c("bootstrap","validation")
      val.results <- results$validation
      metrics <- lapply(val.results, function(l) l$metrics) %>% rlist::list.rbind(); metrics %>%head()

      avgMSE <- metrics %>% as_tibble %>% group_by(depth) %>%
        select(MSE) %>%
        summarise_all(mean)

      avgMSE
      avgMSE <- avgMSE$MSE[avgMSE$depth == depths[s]]
      avgMSE
      over_var <- var.res + avgMSE

      # export overall variance raster
      writeRaster(over_var, filename = paste0(res.dir,var_name,"_overVAR.tif"), overwrite = T)

      # standar deviation
      sd.map <- sqrt(over_var)

      # export overall variance raster
      writeRaster(sd.map, filename = paste0(res.dir,var_name,"_SD.tif"), overwrite = T)
    }
    if (model == "QRF"){
      b.files <- list.files(res.dir.depth, pattern = "q0.5", full.names = T);b.files
      avgmap <- mean(stack(b.files))
      writeRaster(avgmap, str_c(res.dir,var_name,"_median.tif"))
    }

    ## -------------calculate prediction intervals----------------------

    if (model == "RF"){
      # percentile z value for normal distribution
      qp <- qnorm(0.95)
      #standar error
      se.map <- sd.map * qp

      # upper limit interval
      ulim_map <- avgmap + se.map
      # export overall variance raster
      writeRaster(ulim_map, filename = paste0(res.dir,var_name,"_Ulim.tif"), overwrite = T)

      # lower limit interval
      llim_map <- avgmap - se.map
      # export overall variance raster
      writeRaster(llim_map, filename = paste0(res.dir,var_name,"_Llim.tif"), overwrite = T)

      # range of prediction
      range_map <- ulim_map - llim_map
      # export overall variance raster
      writeRaster(range_map, filename = paste0(res.dir,var_name,"_PIRange.tif"), overwrite = T)
    }
  }
}
    
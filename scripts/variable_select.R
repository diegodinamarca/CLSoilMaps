# Package VSURF  -   Selecci?n de variables
# install.packages("VSURF")
library(doParallel)
# library(Boruta)
library(tidyverse)
library(VSURF)

rm(list = ls())


variables <- c("Arcilla","arena","Da","MO")

system.time({
  for (i in 1:3) {
    var <- variables[i]
    files <- list.files(path = "./materiales/Database/", pattern = paste0(var,".csv"), full.names = T);files
    
    bd <- read.csv(files)
    
    # change data type to factor    
    # bd$terrain_surface_classification_iwahashi <- as.factor(bd$terrain_surface_classification_iwahashi)
    bd$map_geo_chile <- as.factor(bd$map_geo_chile)
    bd$curvature_classification <- as.factor(bd$curvature_classification)
    # check data type
    # str(bd, list.len= ncol(bd))
    # View(bd)
    #Sacar columna ID
    bd <- bd[, -2]
    # index <- grep(pattern = "Easting", names(bd));index
    # bd <- bd[,-index]
    # index <- grep(pattern = "Northing", names(bd));index
    
    # bd <- bd[1:100, c(1:22)] 
    res <- list()
    df <- bd[ complete.cases(bd), ] #eliminar filas con NA
    # bd[ !complete.cases(bd), ] %>% as_tibble %>% write_csv("missing.csv")
    # colSums(is.na(bd)) %>% as.data.frame()
    # df %>% dplyr::filter(depth >= 22.5)
    df <- df %>% dplyr::filter(depth > 22.5)
    # df <- df[1:10,]
    
    y.var <- df[ ,4]
    x.vars <- df[ ,c(1,6:ncol(df)) ]
    #variable selection using random forest
    thres <- VSURF_thres(x = x.vars, y = y.var, ntree = 2000,
                         mtry = max(floor(ncol(x.vars)/3), 1), nfor.thres = 50, nmin = 1,
                         RFimplem = "ranger", parallel = TRUE, clusterType = "PSOCK",
                         ncores = parallel::detectCores() - 1, verbose = TRUE)
    tune
    # data.frame(imp = thres$imp.mean.dec) %>% summary()
    res$min.thres <- thres$min.thres
    if (floor(ncol(x.vars)*75/100) < thres$num.varselect.thres){
      q <- quantile(thres$imp.mean.dec)
      thres <- tune(thres, nmin = q[[3]]/thres$min.thres)
    }
    # data.frame(nombre = names(x.vars[thres$imp.mean.dec.ind]), importance = thres$imp.mean.dec) %>%
    #   write_csv("importance.csv")
    
    data.frame(imp = thres$imp.mean.dec) %>%
      ggplot(aes(imp)) +
      geom_histogram()
      
    
    # thres.tuned$num.varselect.thres
    
    interp <- VSURF_interp(x = x.vars, y = y.var, ntree = 2000, vars = thres$varselect.thres,
                           nfor.interp = 25, nsd = 1, RFimplem = "ranger",
                           parallel = TRUE, ncores = detectCores() - 1,
                           clusterType = "PSOCK", verbose = TRUE)
    
    # interp <- tune(interp, nsd = 100)
    # interp.tuned$num.varselect.interp
    interp$num.varselect.interp
    pred <- VSURF_pred(x = x.vars, y = y.var, ntree = 200, err.interp = interp$err.interp,
                       varselect.interp = interp$varselect.interp, nfor.pred = 25, nmj = 1,
                       RFimplem = "ranger", parallel = TRUE, ncores = detectCores() - 1, verbose = TRUE)
    
    res$thres <- thres
    res$interp <- interp
    res$pred <- pred
    
    if (is.null(pred$varselect.pred)){
      #variables para prediccion
      res$pred_names <- names(x.vars[res$pred$varselect.interp])
    }else{
      res$pred_names <- names(x.vars[res$pred$varselect.pred])
    }
    #variables ordenadas segun su importancia
    res$imp_names <- names(x.vars[res$thres$imp.mean.dec.ind])
    #variables para interpretacion
    res$interp_names <- names(x.vars[res$interp$varselect.interp])
    #guardar nombre de la variable
    res$y.name <- names(df[4])
    #bd usada para seleccionar variables
    res$data <- df
    saveRDS(res, file = paste0("D:/paper_BDsuelos/resultados/var_select/",res$y.name,"_lower.rds"))
    res
    
    ########## LOWER: DATOS 30-200CM ##########
    
    res <- list()
    df <- bd[ complete.cases(bd), ] #eliminar filas con NA
    # bd[ !complete.cases(bd), ] %>% as_tibble %>% write_csv("missing.csv")
    # colSums(is.na(bd)) %>% as.data.frame()
    # df %>% dplyr::filter(depth >= 22.5)
    df <- df %>% dplyr::filter(depth <= 22.5)
    y.var <- df[ ,4]
    x.vars <- df[ ,c(1,6:ncol(df)) ]
    #variable selection using random forest
    thres <- VSURF_thres(x = x.vars, y = y.var, ntree = 2000,
                         mtry = max(floor(ncol(x.vars)/3), 1), nfor.thres = 50, nmin = 1,
                         RFimplem = "ranger", parallel = TRUE, clusterType = "PSOCK",
                         ncores = parallel::detectCores() - 1, verbose = TRUE)
    
    # data.frame(imp = thres$imp.mean.dec) %>% summary()
    res$min.thres <- thres$min.thres
    if (floor(ncol(x.vars)*75/100) < thres$num.varselect.thres){
      q <- quantile(thres$imp.mean.dec)
      thres <- tune(thres, nmin = q[[3]]/thres$min.thres)
    }
    
    # data.frame(nombre = names(x.vars[thres$imp.mean.dec.ind]), importance = thres$imp.mean.dec) %>%
    #   write_csv("importance.csv")
    
    data.frame(imp = thres$imp.mean.dec) %>%
      ggplot(aes(imp)) +
      geom_histogram()
    
    # thres.tuned$num.varselect.thres
    
    interp <- VSURF_interp(x = x.vars, y = y.var, ntree = 2000, vars = thres$varselect.thres,
                           nfor.interp = 25, nsd = 1, RFimplem = "ranger",
                           parallel = TRUE, ncores = detectCores() - 1,
                           clusterType = "PSOCK", verbose = TRUE)
    
    # interp <- tune(interp, nsd = 100)
    # interp.tuned$num.varselect.interp
    # interp$num.varselect.interp
    pred <- VSURF_pred(x = x.vars, y = y.var, ntree = 2000, err.interp = interp$err.interp,
                       varselect.interp = interp$varselect.interp, nfor.pred = 25, nmj = 1,
                       RFimplem = "ranger", parallel = TRUE, ncores = detectCores() - 1, verbose = TRUE)
    
    res$thres <- thres
    res$interp <- interp
    res$pred <- pred
    
    if (is.null(pred$varselect.pred)){
      #variables para prediccion
      res$pred_names <- names(x.vars[res$pred$varselect.interp])
    }else{
      res$pred_names <- names(x.vars[res$pred$varselect.pred])
    }
    #variables ordenadas segun su importancia
    res$imp_names <- names(x.vars[res$thres$imp.mean.dec.ind])
    #variables para interpretacion
    res$interp_names <- names(x.vars[res$interp$varselect.interp])
    #guardar nombre de la variable
    res$y.name <- names(df[4])
    #bd usada para seleccionar variables
    res$data <- df
    saveRDS(res, file = paste0("D:/paper_BDsuelos/resultados/var_select/",res$y.name,"_upper.rds"))
    res
    # 
    # var.names <- list()
    # var.selected <- list()
    # 
    # for (i in 1:length(vsurf_res)){
    #   v <- vsurf_res[[i]]
    #   var.selected <- append(var.selected, list(v$pred_names))
    #   var.names <- append(var.names, v$y.name)
    # }
    # 
    # var.names <- unlist(var.names)
    # # max number of uncorrelated variables per depth
    # maxrow <- max(unlist(lapply(var.selected, length)))
    # # empty matrix for exporting results
    # m <- matrix(NA, nrow = maxrow, ncol = 6)
    # # fill matrix
    # for (i in 1:6){
    #   vec <- var.selected[[i]]
    #   l <- length(vec)
    #   m[1:l, i] <- vec
    # }
    # colnames(m) <- var.names
    # # export results
    # write.csv(m, paste0("./Variable_selection/",var,".csv"))
    # saveRDS(vsurf_res, file = paste0("./Variable_selection/",var,"_VSURF.rds"))
    
  }
  
})

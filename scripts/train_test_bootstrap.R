####################FIT TRAIN, TUNE AND PREDICT MODEL WITH UNIFIED BD############################
#borrar variables cargadas
rm(list = ls())

#Library
# install.packages("conflicted")
library(conflicted)
library(tidymodels)
library(SpatialML)
library(doParallel)
library(tidyverse)
library(furrr)

# source("./SpatialML/R/predict.grf.R")
# source("./SpatialML/R/grf.R")

conflict_prefer("filter","dplyr")
conflict_prefer("select","dplyr")
conflict_prefer("tune", "tune")
conflict_prefer("group_by", "dplyr")

################################### FUNCIONES #######################################################
source("./scripts/bootstrap_functions.R")

################################### WORKFLOW ############################################

# Input parameters
variables <- c("Da","Arcilla","arena")
model = "RF" # options: RF, QRF, GRF
param.metric = "rmse" #options: rmse, rsq, mpe
boot.n <- 100 #number of bootstraps
sample.bd <- 0 #number of sampled data points, 0 for all.
nthreads <- 15

#selected variables from the lower or upper half of the DB
selection <- c("lower_uncor","upper_uncor") 

for (v in 1:3){
  var <- variables[v]
  var_name <- var
  for (s in 1:2){
    
    sel <- selection[s]
    
    # load data
    file.path <- paste(getwd(),"materiales","Database",str_c(var, ".csv"), sep = "/");file.path
    bd <- read_csv(file = file.path) %>%
      dplyr::select(-c(2));bd
    
    bd$map_geo_chile = as_factor(bd$map_geo_chile)
    bd$curvature_classification = as_factor(bd$curvature_classification)
    bd$comp_geologica = as_factor(bd$comp_geologica)
    bd$comp_geologica_2 = as_factor(bd$comp_geologica_2)
    bd$hydro_soil_group = as_factor(bd$hydro_soil_group)
    bd$runoff_potential = as_factor(bd$runoff_potential)
    bd_clean <- bd
    bd_clean <- bd_clean %>% rename(x = 2, y= 3)
    
    #Load predictors
    files <- list.files(path = "./resultados/var_select/", pattern = "\\.rds$", full.names = T);files
    f <- files[grep(pattern = paste0(var,"_",sel), files)];f
    var.sel <- read_rds(f)
    
    pred_names <- var.sel$pred_names;pred_names

    bd_clean <- bd_clean %>% dplyr::select(c(1:4,pred_names))
    bd_clean <- bd_clean[complete.cases(bd_clean),]
    
    # force depth as a covariate
    if (!("depth" %in% pred_names)) pred_names <- c(pred_names, "depth")
    y <- colnames(bd_clean)[4];y
    bd_clean <- bd_clean[,c(2:ncol(bd_clean),1)]
    
    # set formula
    x.nam <- str_flatten(pred_names, " + ")
    model.form <- as.formula(paste0(y,"~",x.nam));model.form
    
    {
      #sample database
      if (sample.bd != 0){
        bd_sample <- bd_clean %>% slice_sample(n = sample.bd)
        bd_clean <- bd_sample
      }

      ## train/test set----
      set.seed(1234)
      bd_split <- initial_split(bd_clean)
      bd_train <- training(bd_split)
      bd_test <- testing(bd_split)

      # Training models
      boot.samples <- bootstraps(bd_train, times = boot.n)

      # default parameters
      mtry <- floor(length(pred_names)/3);mtry
      ntree <- 500
      min_n <- 5
    }
    # export train and test set
    bd_train %>% write_csv(str_c("./",var,"_",sel,"_train.csv"))
    bd_test %>% write_csv(str_c("./",var,"_",sel,"_test.csv"))
    
# Tune parameters----
dir.create("./resultados/param_tunning/")
pt <- str_c("./resultados/param_tunning/",var,"_",sel,"_tunning_results.rds")
not.exists <- T
tryCatch({
  rf_tune_results <- read_rds(pt)
  not.exists = F
  cat("reading tuned parameters...Done\n")
},
error = function(e){
  cat("No se encuentra el archivo en la ruta: ", pt,"\n")
  not.exists = T
},
warning = function(w){
  not.exists = T
  cat("No se encuentra el archivo en la ruta: ", pt,"\n")
})

if (not.exists){
  cat("Tunning ", var,"_",sel, " parameters\n")
  
  # 10-fold cross validation splits
  set.seed(1234)
  bd_cv <- vfold_cv(bd_clean, v = 10)

  ## Define the recipe
  bd_recipe <- recipe(model.form, data = head(bd_clean))
  prepped_recipe <- prep(bd_recipe, training = bd_clean)

  ## Selecting the model and parameters
  rf_model <-
    # specify that the model is a random forest
    rand_forest() %>%
    # specify parameters that need to be tuned
    set_args(mtry = tune(), trees = tune(), min_n = tune()) %>%
    # select the engine/package that underlies the model
    set_engine("ranger", importance = "impurity") %>%
    # choose either the continuous regression or binary classification mode
    set_mode("regression")


  # Workflow
  # set the workflow
  rf_workflow <- workflow() %>%
    # add the recipe
    add_recipe(prepped_recipe) %>%
    # add the model
    add_model(rf_model)


  # Tune parameters
  nro.pred  <- terms(model.form) %>%
    attr("term.labels") %>% 
    length()
  # specify which values to try
  rf_grid <- expand.grid(mtry = seq(floor(nro.pred/3), floor(nro.pred*0.75)),
                         min_n = c(3, 4, 5, 6, 7, 8, 9, 10),
                         trees = c(500,1000))

  # tune model in parallel
  ncores <- detectCores()-1
  cl <- makePSOCKcluster(ncores)
  registerDoParallel(cl)
  system.time(
    rf_tune_results <- rf_workflow %>%
      tune_grid(resamples = bd_cv, #CV object
                grid = rf_grid, # grid of values to try
                metrics = metric_set(rmse, rsq) # metrics we care about
      )
  )
  stopCluster(cl)

  # create output directory
  out <- "./resultados/param_tunning"
  out <- str_c(out, y, sep = "/");out
  dir.create(out, showWarnings = F)

  rf_tune_results %>% collect_metrics()

  rf_tune_results %>% autoplot() %>%
    ggsave(filename = str_c("./resultados/param_tunning/plots/",y,".parameter_tunning.png"), width = 14, height = 10)

  rf_tune_results %>% show_best("rmse") %>%
    bind_rows(rf_tune_results %>% show_best("rsq")) %>%
    write_csv( str_c(out,"/",y,".tunning_results.csv"))

  write_rds(rf_tune_results, str_c("./resultados/param_tunning/",var,"_",sel,"_tunning_results.rds"))

  cat("Finished tunning parameters\n")
}

# Select best parameters
best_param <- rf_tune_results %>%
  select_best(metric = param.metric)

    mtry <- best_param$mtry;mtry
    ntree <- best_param$trees;ntree
    min_n <- best_param$min_n;min_n
    
    # Fit model-----
    print("Iniciando entrenamiento del modelo...")
    print(paste0("variable: ",y," ",sel))
    
    dir.path = str_c(getwd(),"resultados","modelos",model,var,str_c(y,"_",sel), sep = "/");dir.path
    unlink(list.files(dir.path, full.names = TRUE), recursive = FALSE)
    dir.create(path = dir.path, showWarnings = F)
    
    # fit models in parallel
    time <- system.time({
      # Enable parallel processing
      plan(multisession, workers= nthreads)
      boot.results <- future_map(boot.samples$splits, bootstrap_fit, formula = model.form, method = model,
                                 ntree= ntree, mtry = mtry, min_n = min_n, path = dir.path, nthreads = 1, .options = furrr_options(seed=T))  
    }
    )
    
    cat("model training time",time[[3]]/60,"min\n")
    
    model.list <- list.files(path = dir.path, full.names = T);last(model.list)
    
    # validation with retained data
    val.results <- future_map(model.list, bootstrap_predict_uni, data = bd_test, method = model, .options = furrr_options(seed=T))
    
    results <- list(
      calibration <- boot.results,
      validation <- val.results,
      model.form <- model.form
    )
    
    write_rds(results, str_c("./resultados/modelos/",model,"_",var,"_",sel,"_results.rds"))
    
    ############################# Summarise results #############################
    str_c("./resultados/modelos/",model,"_",var,"_",sel,"_results.rds")
    results <- read_rds(str_c("./resultados/modelos/",model,"_",var,"_",sel,"_results.rds"))
    
    names(results) <- c("bootstrap","validation")
    val.results <- results$validation
    
    metrics <- lapply(val.results, function(l) l$metrics) %>% rlist::list.rbind(); metrics %>%head()
    mean.metrics <- metrics %>% as_tibble %>% group_by(depth) %>%
      select(RMSE, r, R2, PBIAS = PBIAS.., NRMSE= NRMSE..) %>%
      summarise_all(mean)
    mean.metrics %>% write_csv(str_c("./resultados/",var,"_",sel,"_metrics.csv"))
    
    predictions <- lapply(val.results, function(l) l$predictions) %>% rlist::list.rbind();predictions
    
    # some names for plots
    depths.nam <- c("2.5" = "0-5cm" ,"10" = "5-15cm", "22.5" = "15-30cm","45" = "30-60cm","80" = "60-100cm","150" = "100-200cm")
    depths <- c(2.5, 10, 22.5, 45, 80, 150)
    var.eng <- c(Da = "Bulk Density", Arcilla = "Clay", Limo = "Silt", arena = "Sand")
    
    pred.list <- list(rep(NA, 6))
    for (i in 1:6){
      # i = 1
      pred.depth <- predictions %>% dplyr::filter(depth == depths[i])
      
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
        
        pred.list[[i]] <- bind_cols(pred.means, pred.var) %>% select(c(2, 1, 5, 3, 4))
      }
      
    }
    pred.stats <- rlist::list.rbind(pred.list);pred.stats
    {
      bd <- read_csv(str_c("./materiales/Database/",var,".csv"))
      fuentes <-  bd$fuente[match(pred.stats$obs,bd[[5]])]
      fuentes[fuentes == 1] <- "UChile"
      fuentes[fuentes == 2] <- "CIREN"
      fuentes[fuentes == 3] <- "CHLSOC"
      fuentes[fuentes == 4] <- "WoSIS"
      fuentes[fuentes == 5] <- "Otros"
      pred.stats$fuente <- fuentes
    }
    pred.stats
    
    if (length(which(is.na(pred.stats$fuente))) != 0 ) pred.stats$fuente[which(is.na(pred.stats$fuente))] <- "CIREN"
    
    pred.stats %>% write_csv(paste0("./resultados/",var,"_",sel,"_predictions.csv"))
    
  }
}

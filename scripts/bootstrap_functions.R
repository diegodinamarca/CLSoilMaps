



bootstrap_fit <- function(bsample, formula, method = "RF", ntree= 500, mtry, min_n = 5, path = getwd(), ...){
  # browser()
  train_data <- analysis(bsample)
  coords <- train_data %>% select(x,y)
  test_data <- assessment(bsample)
  bw = floor(nrow(train_data)/10)
  
  dots <- list(...)
  arg_names <- names(dots)
  
  if (length(dots) != 0){
    if ("nthreads" %in% arg_names){
      nthreads = dots$nthreads
    } else{
      nthreads = 1
    }
    if ("quantiles" %in% arg_names){
      quantiles = dots$quantiles
    } else {
      quantiles = c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525, 0.5,
                    0.475, 0.45, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.005)
    }
  }
  
  
  if (method == "GRF"){
    cat("Fitting Geographical Regression Forest", bsample$id[[1]], "\n")
    grf.model <- grf(formula = formula, train_data, kernel = "adaptive",
                     bw = bw, coords = coords, ntree = 500,
                     mtry = mtry, min_n = min_n, forests = T)
    
    write_rds(grf.model, stringr::str_c(path,"/GRF.",bsample$id,".rds"))
    
    
    preds.local.w25 <- predict.grf(object = grf.model, new.data = test_data,
                         x.var.name = "x", y.var.name = "y", local.w = 0.25, global.w = 0.75)
    preds.local.w50 <- predict.grf(object = grf.model, new.data = test_data,
                         x.var.name = "x", y.var.name = "y", local.w = 0.5, global.w = 0.5)
    preds.local.w75 <- predict.grf(object = grf.model, new.data = test_data,
                         x.var.name = "x", y.var.name = "y", local.w = 0.75, global.w = 0.25)
    
    return(
      list(
        metrics = tibble(
          localw.25 = preds.local.w25 %>% hydroGOF::gof(obs = test_data[[3]], norm = "maxmin") %>% t(),
          localw.50 = preds.local.w50 %>% hydroGOF::gof(obs = test_data[[3]], norm = "maxmin") %>% t(),
          localw.75 = preds.local.w75 %>% hydroGOF::gof(obs = test_data[[3]], norm = "maxmin") %>% t()
        ),
        predictions = tibble(
          localw.25 = preds.local.w25,
          localw.50 = preds.local.w50,
          localw.75 = preds.local.w75,
          obs = test_data[[3]]
        )
      )
    )
  } else if (method == "QRF"){
    cat("Fitting Quantile Regression Forest", bsample$id[[1]], "\n")
    qrf_model <- ranger::ranger(formula = formula, data = train_data, num.trees = ntree, mtry = mtry-2,
                       importance = "impurity", write.forest = TRUE, min.node.size = min_n, 
                       quantreg = TRUE, oob.error = TRUE, num.threads = nthreads, verbose = TRUE,
                       seed = NULL, dependent.variable.name = NULL, x = NULL, y = NULL)
    
    write_rds(qrf_model, stringr::str_c(path,"/QRF.",bsample$id,".rds"))
    
    preds <- predict(qrf_model, data = test_data, type="quantiles", quantiles=quantiles)$predictions %>%
      as_tibble
    names(preds) <- str_c("q",as.character(quantiles))
    return(
      list(
        metrics = preds[, 11] %>% hydroGOF::gof(obs = test_data[[3]], norm = "maxmin") %>% t() %>% as_tibble,
        predictions = tibble(
          preds,
          obs = test_data[[3]]
        )
      )
    )
  } else{
    cat("Fitting Random Forest", bsample$id[[1]], "\n")
    rf_model <- ranger::ranger(formula = formula, data = train_data, num.trees = ntree, mtry = mtry,
                        importance = "impurity", write.forest = TRUE, min.node.size = min_n, 
                        quantreg = FALSE, oob.error = TRUE, num.threads = nthreads, verbose = TRUE,
                        seed = NULL, dependent.variable.name = NULL, x = NULL, y = NULL)
    
    write_rds(rf_model, stringr::str_c(path,"/RF.",bsample$id,".rds"))
    preds <- predict(rf_model, data = test_data)$predictions
    return(
      list(
        metrics = preds %>% hydroGOF::gof(obs = test_data[[3]], norm = "maxmin") %>% t() %>% as_tibble,
        predictions = tibble(
          predictions = preds,
          obs = test_data[[3]]
        )
      )
    )
  }
  
}

bootstrap_predict <- function(model, data, method = "RF", ...){
  # browser()
  dots <- list(...)
  arg_names <- names(dots)
  
  if (length(dots) != 0){
    if ("quantiles" %in% arg_names){
      quantiles = dots$quantiles
    } else {
      quantiles = c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525, 0.5,
                    0.475, 0.45, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.005)
    }
  } else {
    quantiles = c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525, 0.5,
                  0.475, 0.45, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.005)
  }
  
  if (method == "GRF"){
    grf.model <- read_rds(model)
    preds.local.w25 <- predict.grf(object = grf.model, new.data = data,
                                   x.var.name = "x", y.var.name = "y", local.w = 0.25, global.w = 0.75)
    
    preds.local.w50 <- predict.grf(object = grf.model, new.data = data,
                                   x.var.name = "x", y.var.name = "y", local.w = 0.5, global.w = 0.5)
    
    preds.local.w75 <- predict.grf(object = grf.model, new.data = data,
                                   x.var.name = "x", y.var.name = "y", local.w = 0.75, global.w = 0.25)
    return(
      list(
        metrics = tibble(
          localw.25 = preds.local.w25 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t(),
          localw.50 = preds.local.w50 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t(),
          localw.75 = preds.local.w75 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t()
        ),
        predictions = tibble(
          localw.25 = preds.local.w25,
          localw.50 = preds.local.w50,
          localw.75 = preds.local.w75,
          obs = data[[3]]
        )
      )
    )
  } else if (method == "QRF"){
    qrf_model <- read_rds(model)
    
    preds <- predict(qrf_model, data = data, type="quantiles", quantiles=quantiles)$predictions %>%
      as_tibble
    names(preds) <- str_c("q",as.character(quantiles))
    return(
      list(
        metrics = preds[, 11] %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t() %>% as_tibble,
        predictions = tibble(
          preds,
          obs = data[[3]]
        )
      )
    )
  } else {
    rf_model <- read_rds(model)
    
    preds <- predict(rf_model, data = data)$predictions
    return(
      list(
        metrics = preds %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t() %>% as_tibble,
        predictions = tibble(
          predictions = preds,
          obs = data[[3]]
        )
      )
    )
  }
  
  
}


# 
# model.list <- list.files(path = dir.path, full.names = T);model.list
# model <- model.list[1]
# data = bd_test
# method = "QRF"
bootstrap_predict_uni <- function(model, data, method = "RF", ...){
  # browser()
  require(ranger)
  dots <- list(...)
  arg_names <- names(dots)
  
  if (length(dots) != 0){
    if ("quantiles" %in% arg_names){
      quantiles = dots$quantiles
    } else {
      quantiles = c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525, 0.5,
                    0.475, 0.45, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.005)
    }
  } else {
    quantiles = c(0.995, 0.9875, 0.975, 0.95, 0.9, 0.8, 0.7, 0.6, 0.55, 0.525, 0.5,
                  0.475, 0.45, 0.4, 0.3, 0.2, 0.1, 0.05, 0.025, 0.0125, 0.005)
  }
  
  if (method == "GRF"){
    grf.model <- read_rds(model)
    
    depths <- data[["depth"]] %>% unique
    pred.df <- data.frame()
    metrics.df <- data.frame(metrics = numeric(0), depth = numeric(0))
    
    for (i in 1:length(depths)){
      # i = 1
      data.depth <- data %>% filter(depth == depths[i])
      preds.local.w25 <- predict.grf(object = grf.model, new.data = data.depth,
                                     x.var.name = "x", y.var.name = "y", local.w = 0.25, global.w = 0.75)
      
      preds.local.w50 <- predict.grf(object = grf.model, new.data = data.depth,
                                     x.var.name = "x", y.var.name = "y", local.w = 0.5, global.w = 0.5)
      
      preds.local.w75 <- predict.grf(object = grf.model, new.data = data.depth,
                                     x.var.name = "x", y.var.name = "y", local.w = 0.75, global.w = 0.25)
      localw.25 = preds.local.w25 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t()
      localw.50 = preds.local.w50 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t()
      localw.75 = preds.local.w75 %>% hydroGOF::gof(obs = data[[3]], norm = "maxmin") %>% t()
      
      pred.df <- rbind(pred.df, data.frame(preds.local.w25, preds.local.w50, preds.local.w75,
                                           obs = data.depth[[3]], depth = depths[i]))
      metrics.df <- rbind(metrics.df, data.frame(localw.25, localw.50, localw.75, depth = depths[i]))
    }
    

    return(
      list(
        metrics = tibble(
          metrics.df
        ),
        predictions = tibble(
          pred.df
        )
      )
    )
  } else if (method == "QRF"){
    qrf_model <- read_rds(model)
    
    depths <- data[["depth"]] %>% unique
    pred.df <- data.frame()
    metrics.df <- data.frame(metrics = numeric(0), depth = numeric(0))
    
    for (i in 1:length(depths)){
      # i = 3
      data.depth <- data %>% filter(depth == depths[i])
      preds <- predict(qrf_model, data = data.depth, type="quantiles", quantiles=quantiles)$predictions
      # names(preds) <- str_c("q",as.character(quantiles))
      metrics = preds[, 11] %>% hydroGOF::gof(obs = data.depth[[3]], norm = "maxmin") %>% t() %>% as_tibble
      
      pred.df <- rbind(pred.df, data.frame(preds, obs = data.depth[[3]], depth = depths[i]));pred.df
      metrics.df <- rbind(metrics.df, data.frame(metrics, depth = depths[i]));metrics.df
    }
    names(pred.df)[1:21] <- str_c("q",as.character(quantiles))
    return(
      list(
        metrics = metrics.df %>% as_tibble,
        predictions = tibble(pred.df)
      )
    )
  } else {
    rf_model <- read_rds(model)
    rf_model
    
    depths <- data[["depth"]] %>% unique
    pred.df <- data.frame(predictions = numeric(0), obs = numeric(0), depth = numeric(0))
    metrics.df <- data.frame(metrics = numeric(0), depth = numeric(0))
    for (i in 1:length(depths)){
      # i = 1
      data.depth <- data %>% filter(depth == depths[i])
      preds <- predict(rf_model, data = data.depth)$predictions
      metrics = preds %>% hydroGOF::gof(obs = data.depth[[3]], norm = "maxmin") %>% t() %>% as_tibble
      
      pred.df <- rbind(pred.df, data.frame(predictions = preds, obs = data.depth[[3]], depth = depths[i]))
      metrics.df <- rbind(metrics.df, data.frame(metrics, depth = depths[i]))
    }
    pred.df
    metrics.df
    return(
      list(
        metrics = metrics.df,
        predictions = tibble(pred.df)
      )
    )
  }
  
}
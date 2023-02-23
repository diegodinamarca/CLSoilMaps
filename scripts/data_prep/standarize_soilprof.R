stand_soilatt = function(obs.data, var, lambda, vlow = 0, vhigh = 1){
  require(aqp)
  require(ithir)
  #Define SoilProfile database
  depths(obs.data) = ID ~ top + bottom
  
  #smoothing parameter
  lambda.l <- c(10, 1, 0.1, 0.01, 0.001,0.0001,0.00001)
  
  #GET RMSE for each lambda
  rmse_models <- sapply(lambda.l, function(x){
    tryCatch({
      # x <- 10
      eaFit <- ea_spline(obs.data, var.name = var, #CAMBIAR COLUMNA SEGUN PROPIEDAD
                         d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = x, vlow = vlow, vhigh = vhigh,
                         show.progress = TRUE)
      print(paste0("lambda: ",x, " RMSE: ",round(eaFit$splineFitError$rmse %>% mean,3)))
      obs <- as.numeric(eaFit$obs.preds[[4]])
      pred <- eaFit$obs.preds$predicted
      rmse <- RMSE(pred, obs)
      res <- rmse},
      error = function(w){
        print(paste0("Error fitting splines for lambda = ", x))
        res <- 9999
      })
  })
  # Get final results
  eaFit <- ea_spline(obs.data, var.name = var,
                     d = t(c(0, 5, 15, 30, 60, 100, 200)), lam = lambda, vlow = vlow, vhigh = vhigh,
                     show.progress = F)
  return(eaFit)
}
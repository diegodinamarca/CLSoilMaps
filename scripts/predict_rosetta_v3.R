## ---------------------------

## load up the packages we will need:  (uncomment as required)

require(tidyverse)
require(terra)
require(sf)
require(soilDB)
require(soilphysics)
library(future.apply)
library(progressr)
library(soiltexture)

## ---------------------------
##First read in the arguments listed at the command line
# args=(commandArgs(TRUE))
# 
# for(i in 1:length(args)){
#   eval(parse(text=args[[i]]))
# }

## ---------------------------

main <- function(depth){
  i = depth
  outdir <- paste0(getwd(), "/PTF_results_2")
  dir.create(outdir)
  
  imgs.files <- list.files("D:/paper_BDsuelos/Versiones/Version3/", full.names = T, pattern = ".tif$") 
  # imgs.files <- list.files("./resultados/100m_v3/", full.names = T, pattern = ".tif$") 
  imgs <- rast(imgs.files);names(imgs)
  names(imgs)[19:24] <- str_replace(string = names(imgs)[19:24], pattern = "Arcilla","Limo")
  
  depths <- c(2.5, 10, 22.5, 45, 80, 150)
  depth.width <- c(5,10,15,30,40,100)
  
  PTF_soilmaps <- function(i){
    cat("Procesando profundidad ",depths[i], "\n")
    index <- grep(names(imgs), pattern = paste0("X",depths[i]));imgs[[index]] %>% names
    return(imgs[[index]])
  }
  
  imgs.d <- PTF_soilmaps(i)
  ##########################
  # crear tiles de los mapas
  tiledir <- paste0(getwd(),"/maptiles_",depths[i])
  if (length(list.files(tiledir)) == 0){
    dir.create(tiledir)
    tileframes <- rast(ncols=1, nrows = 50)
    ext(tileframes) <- ext(imgs)
    tiles <- makeTiles(x = imgs.d, tileframes, filename = paste0(tiledir,"/tile_.tif"), overwrite = TRUE)
  }else{
    tiles <- list.files(tiledir, full.names = TRUE, pattern = ".tif$")
  }
  
  get_unprocessed_tiles <- function(tiles, outdir){
    pred.files <- list.files(paste0(outdir,"/AWC"))
    # numero de tiles completados
    tiles.comp <- substr(basename(pred.files), start = 1, stop = nchar(basename(pred.files))-4) %>% strsplit(split = "_") %>% 
      lapply(function(x){
        x[length(x)]
      }) %>%
      unlist %>% 
      sort
    if (length(tiles.comp) == 0){
      return(tiles)
    }else{
      # get index of processed tiles
      index.comp <- lapply(tiles.comp, function(x){grep(paste0("_",x,"\\.tif"), tiles)}) %>% unlist
      # get unprocessed tiles
      cat("tiles left: ",length(tiles[-index.comp]))
      return(tiles[-index.comp])
    }
    
  }
  
  tiles <- get_unprocessed_tiles(tiles, outdir)
  
  ##########################
  
  # iterar para los n tiles
  plan(sequential)
  handlers("progress")
  with_progress({
    p <- progressor(along = tiles)
    future_lapply(tiles, function(x){
      p()
      predict_tile_rosetta(x, outdir, i)
    })
  }) 
}

####################################################################.

predict_tile_rosetta <- function(tile_filename, outdir, i){
  # Function for predicting a tile
  # tile_filename:  multiband image of soil maps. Bands of the raster file 
  #   should correspond to "Clay","Sand","Bulkd density" and "Silt", in that order.
  # Texture values are normalized so the sum is equal to 100.
  # outdir: output directory
  # i: position of the soil horizon predicted from 1 to 6. It is used
  #   only to name output files
  
  # tile_filename <- "D:/PTFs/maptiles_10/tile_20.tif"
  depths <- c(2.5, 10, 22.5, 45, 80, 150)
  depth.width <- c(5,10,15,30,40,100)
  
  imgs.d <- rast(tile_filename)
  tilename <- substr(basename(tile_filename), start = 1, 
                     stop = (nchar(basename(tile_filename))-4)) 
  
  # Transform the image to a matrix
  soil.val <- terra::values(imgs.d);colnames(soil.val)
  colnames(soil.val) <- c("Clay", "Sand", "Bulk_density","Silt")
  vars <- c("Sand", "Silt", "Clay","Bulk_density") 

  ####################################################################.
  # ROSETTA MODEL
  soil <- soil.val %>% 
    as_tibble %>% 
    select(all_of(vars))
  nrow <- nrow(soil)
  index <- which(!complete.cases(soil))
  data.na <- data.frame(Sand = rep(NA, nrow), Silt = rep(NA, nrow), 
                        Clay = rep(NA, nrow), Bulk_density = rep(NA, nrow))
  soil <- drop_na(soil);soil
  if (nrow(soil) == 0){
    cat(tilename, " empty, moving to next tile \n")
    return(TRUE)
  } else{
    message(paste0("processing ",tilename, "\n"))
    
    # Normalize texture values
    tex_norm <- soil %>% select(CLAY = Clay, SILT = Silt, SAND = Sand) %>%
      TT.normalise.sum(residuals = T) %>% as_tibble
    tex_norm
    soil <- soil %>% 
      mutate(Clay = tex_norm$CLAY, Sand = tex_norm$SAND, Silt = tex_norm$SILT)
    
    # #### fUNCIONES AUXILIARES ####
    # create_groups <- function(n, num.groups){
    #   groups <- list()
    #   for (j in 1:num.groups) {
    #     num.datos <- rep(j, n/num.groups)
    #     # groups[[i]] <- rep(i, n/10)
    #     groups[[j]] <- num.datos
    #     if (j == num.groups)groups[[j+1]] = rep(j+1, n%%num.groups)
    #   }
    #   return(unlist(groups))
    # }
    # par_rosetta <- function(dflist, vars){
    #   vars <- c("Sand", "Silt", "Clay","Bulk_density")
    #   # p <- progressor(along = dflist)
    #   future_lapply(dflist, function(x){
    #     # p()
    #     ROSETTA(x, vars = vars, v = "3")
    #     Sys.sleep(1)
    #   })
    # }
    # #### --------------------------
    # 
    # # Setting parallel environment
    # plan(sequential)
    # # plan(multisession, workers = 10)
    # # handlers("progress")
    # 
    # # numero de datos
    # n <- nrow(soil)
    # # cantidad de grupos
    # num.groups = 3000
    # # datos por grupo aprox
    # n/num.groups
    # 
    # # # crear vector de grupos y agregarlo al df
    # groups <- create_groups(n, num.groups)
    # soil$groups <- groups
    # # # dividir el df en los grupos
    # soil.split <- split(soil, f = groups)
    # 
    # # predecir rosetta v3 en paralelo para cada grupo
    # predict_soil_splits <- function(splits){
    #   # get results
    #   results.split <- par_rosetta(splits, vars)
    #   # is nrow == 0?
    #   completed <- as.vector(sapply(results.split, function(x){!is.null(nrow(x))}))
    #   # splits ok
    #   i <- which(completed==TRUE)
    #   # splits notok
    #   # cat("Splits sin resultado: ", j, "\n")
    #   j <- which(completed==FALSE)
    #   if (length(j) != 0){
    #     recalc <- par_rosetta(splits[j])
    #     results.split[j] <- recalc
    #   }
    #   return(results.split)
    # }
    # 
    # results <- predict_soil_splits(soil.split)
    # rv3 <- results %>% rlist::list.rbind()
    
    # Predict Rosetta in chunks of the complete data.frame.
    df.chunk.index <- seq(1,nrow(soil),100)
    pred.data <- data.frame()
    empty.index <- list()
    for (i in 2:length(df.chunk.index)) {
      # for (i in 1290:10000){
        start <- df.chunk.index[i-1]
        end <- df.chunk.index[i]
        data <- soil[start:end, ]
        # print(system.time(results <- ROSETTA(data, vars = vars, v = "3")))
        results <- ROSETTA(data, vars = vars, v = "3")
        cat(i," ")
        # print(results)
        if(length(results) == 0){
          empty.index <- append(empty.index, i)
          dummy.df <- data.frame(Sand = rep(-999, 101), Silt = rep(-999, 101), Clay = rep(-999, 101), Bulkd_density = rep(-999, 101),
                                 theta_r = rep(-999, 101), theta_s = rep(-999, 101), alpha = rep(-999, 101), npar = rep(-999, 101),
                                 ksat = rep(-999, 101))
          pred.data <- bind_rows(pred.data, dummy.df)
        }else{
          results = results %>% select(Sand:ksat)
          pred.data <- bind_rows(pred.data, results)
        }
      }
      cat('\n')
    # beepr::beep(sound = 5)
    start <- last(df.chunk.index)
    end <- nrow(soil)
    data <- soil[start:end, ]
    results <- ROSETTA(data, vars = vars, v = "3")
    cat(i," ")
    if(length(results) == 0){
      empty.index <- append(empty.index, i)
      dummy.df <- data.frame(Sand = rep(-999, 101), Silt = rep(-999, 101), Clay = rep(-999, 101), Bulkd_density = rep(-999, 101),
                             theta_r = rep(-999, 101), theta_s = rep(-999, 101), alpha = rep(-999, 101), npar = rep(-999, 101),
                             ksat = rep(-999, 101))
      pred.data <- bind_rows(pred.data, dummy.df)
    }else{
      results = results %>% select(Sand:ksat)
      pred.data <- bind_rows(pred.data, results)
    }
    # # predict Rosetta for the complete data.frame
    # results <- ROSETTA(soil, vars = vars, v = "3")
    # rv3 <- results
    # results
    
    # ROSETTA V3 Predicted values
    rv3 <- pred.data
    
    # Test in simpler data.frame
    # dftest <- soil[1:10000, ]
    # groups <- create_groups(10000, 10)
    # dftest$groups <- groups
    # dftest.split <- split(dftest, f = groups)
    # plan(multisession, workers = 10)
    # handlers("progress")
    # with_progress(results <- par_rosetta(dftest.split, vars)) 
    # results %>% rlist::list.rbind()
    
    # # Aplicacion funcion usando BD
    # cat("Obteniendo los parametros de Rosetta V3", "\n")
    # tiempo.rosetta <- system.time(rv3 <- ROSETTA(soil.split[[1]][1:100,], vars = vars, v = "3", chunkSize = 10000))
    # cat("Tiempo rosetta: ", tiempo.rosetta,"\n")
    
    # Extracting parameters - Rosetta Version 3
    a_v3 <- 10^rv3[,7] ; n_v3 <- 10^rv3[,8] ; Ks_v3 <- 10^rv3[,9]  
    tr_v3 <- rv3[,5]; ts_v3<- rv3[,6]
    
    # cat("Estimating water retention curve to obtain FC and PWP ","\n")
    # =====  Curva caracteristica ====#
    #### Estimating water retention curve van Genuchten (1980)
    ## function "soilwater" package "soilphysics"
    h1 = 330 #tension FC (hectopascales)
    h2 = 15000 #tension PWP (hectopascales)
    
    tccR_v3  = soilwater(h1, tr_v3, ts_v3, a_v3, n_v3, m = 1 - 1/n_v3,
                         saturation.index = FALSE)
    tpmpR_v3 = soilwater(h2, tr_v3, ts_v3, a_v3, n_v3, m = 1 - 1/n_v3,
                         saturation.index = FALSE)
    
    ####################################################################

# Writing results to raster files -----------------------------------------
    # dummy raster
    r <- rast(imgs.d[[1]])
    dir.create(paste0(outdir, "/CC"))
    dir.create(paste0(outdir, "/PMP"))
    
    #dummy df
    r.data <- data.frame(data = rep(NA, nrow))
    
    if (nrow(rv3) == (nrow(r.data)-length(index))){
      # put values of variables on non NA cells
      r.data[-index,] <- tccR_v3
      r <- setValues(r, r.data[[1]])
      # cat("Exportando CC ","\n")
      fname <- paste0(outdir,"/CC/CC_",depths[i],"_ROSETTA_V",3,"_",tilename,".tif")
      writeRaster(r, fname, overwrite = T)
      
      r.data <- data.frame(data = rep(NA, nrow))
      r.data[-index,] <- tpmpR_v3
      # cat("Exportando PMP ","\n")
      r <- setValues(r, r.data[[1]])
      fname <- paste0(outdir,"/PMP/PMP_",depths[i],"_ROSETTA_V",3,"_",tilename,".tif")
      writeRaster(r, fname, overwrite = T)
      
      # cat("Calculando Ha y AWC ","\n")
      #============= Humedad aprovechable y AWC =======================#
      # Ha = (CC-PMP)*Prof (cm)
      # AWC = (CC - PMP)*100 (%)
      Prof <- depth.width[i]
      
      ### Rosetta V3
      HA_v3  = ((tccR_v3 - tpmpR_v3))*Prof
      AWC_v3 <- (tccR_v3 - tpmpR_v3)*100
      
      dir.create(paste0(outdir, "/HA"))
      dir.create(paste0(outdir, "/AWC"))
      
      r.data <- data.frame(data = rep(NA, nrow))
      r.data[-index,] <- HA_v3
      # cat("Exportando HA ","\n")
      r <- setValues(r, r.data[[1]])
      fname <- paste0(outdir,"/HA/HA_",depths[i],"_ROSETTA_V",3,"_",tilename,".tif")
      writeRaster(r, fname, overwrite = T)
      
      r.data <- data.frame(data = rep(NA, nrow))
      r.data[-index,] <- AWC_v3
      # cat("Exportando AWC ","\n")
      r <- setValues(r, r.data[[1]])
      fname <- paste0(outdir,"/AWC/AWC_",depths[i],"_ROSETTA_V",3,"_",tilename,".tif")
      writeRaster(r, fname, overwrite = T)
      
      # cat("Consolidando BD","\n")
      #Consolidacion bd mas parametros hidraulicos
      soil_final   <- data.frame(tccR_v3,tpmpR_v3,HA_v3, AWC_v3) ## ha y awc  
      names(soil_final)   <- c("Rv3_33","Rv3_1500", "HA_Rv3", "AWC_v3")  
      
      final1 <- cbind(soil,soil_final)
      final <- drop_na(final1)
      
      #Exportacion bd con parametros PTFs
      write.csv(final, paste0(outdir,"/BD_soil_PTF_",depths[i],"_",tilename,".csv")) #cc y pmp
      return(TRUE)
    }else{
      cat("Rosetta: Bad result set for ", tilename, "_", depths[i], "\n")
    }
    
  }
}


main(depth)
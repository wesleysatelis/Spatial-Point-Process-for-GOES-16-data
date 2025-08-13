library(tidyverse)
library(ncdf4)
library(sf)
library(geobr)
library(tmap)
library(terra)
library(parallel)
library(tidyr)
library(arrow)

down_goes <- function(file){
  call <- paste0('aws s3 cp --no-sign-request --no-progress s3://noaa-goes16/', as.character(file), ' files/')
  invisible(system(call))
}

dados_list <- function(ano){
    # ano <- 2023
    dados <- paste0('hd_externo/goes_16/', ano, '/')
    
    caminhos <- list.files(dados, recursive = T, pattern = '*.nc') |> 
        tibble::as_tibble() |> 
        tidyr::separate(value, into=c('dia', 'hora', 'arquivo'), sep = '/', remove = F) |> 
        dplyr::rename(caminho=value) |> 
        dplyr::mutate(caminho=paste0(dados, caminho)) |> 
        dplyr::filter(str_detect(caminho, '.nc?')) |> 
        dplyr::mutate(salva=paste0(str_remove(caminho, '.nc'), ".csv")) |> 
        dplyr::mutate(existe=file.exists(salva))
    
    # deleta <- caminhos |> 
    #     dplyr::filter(existe==T)
    
    # file.remove(deleta$caminho)
    
    write_csv(caminhos, paste0('hd_externo/goes_16/caminhos', ano, '.csv'))
}

crop_2_epsg <- function(raster, shape){
  
    shape <- terra::project(shape, crs(raster))
    raster <- terra::crop(raster, shape) |> 
      terra::mask(shape)
    raster <- terra::project(raster, "EPSG:4674")
    
    return(raster)
}

to_shape <- function(df_caminhos, shape) {
  mclapply(df_caminhos$file, down_goes, mc.cores = 6)
  
  df_caminhos <- df_caminhos |> 
    mutate(file = paste0('files/', basename(file)))
  
  caminho_fogo <- df_caminhos |> 
    filter(produto=='fdcf')
  
  caminho_clear_sky <- df_caminhos |> 
    filter(produto=='acmc')
  
  # -------------------------------------------------------------------
  # Abrir variáveis
  fire_mask <- terra::rast(caminho_fogo$file, subds = "Mask")
  temp      <- terra::rast(caminho_fogo$file, subds = "Temp")
  power     <- terra::rast(caminho_fogo$file, subds = "Power")
  area      <- terra::rast(caminho_fogo$file, subds = "Area")
  dqf       <- terra::rast(caminho_fogo$file, subds = "DQF")
  
  # -------------------------------------------------------------------
  # Crop + mask no CRS original (sem reprojetar ainda)
  shape_proj <- terra::project(shape, terra::crs(fire_mask))
  
  fire_mask <- terra::mask(terra::crop(fire_mask, shape_proj), shape_proj)
  temp      <- terra::mask(terra::crop(temp, shape_proj), shape_proj)
  power     <- terra::mask(terra::crop(power, shape_proj), shape_proj)
  area      <- terra::mask(terra::crop(area, shape_proj), shape_proj)
  dqf       <- terra::mask(terra::crop(dqf, shape_proj), shape_proj)
  
  for(i in 1:2){
    try({
      dqf_clear_sky <- terra::rast(caminho_clear_sky$file[i], subds = "DQF")
      dqf_clear_sky_crop <- terra::mask(terra::crop(dqf_clear_sky, shape_proj), shape_proj)
      
      bcm_clear_sky <- terra::rast(caminho_clear_sky$file[i], subds = "BCM")
      bcm_clear_sky_crop <- terra::mask(terra::crop(bcm_clear_sky, shape_proj), shape_proj)
      })
  }
  
  # -------------------------------------------------------------------
  # Alinhar resolução e grade usando fire_mask como referência
  temp  <- terra::resample(temp, fire_mask, method = "near")
  power <- terra::resample(power, fire_mask, method = "near")
  area  <- terra::resample(area, fire_mask, method = "near")
  dqf   <- terra::resample(dqf, fire_mask, method = "near")
  
  # -------------------------------------------------------------------
  # Reprojetar todos juntos para EPSG:4674
  fire_mask <- terra::project(fire_mask, "EPSG:4674")
  temp      <- terra::project(temp,      "EPSG:4674")
  power     <- terra::project(power,     "EPSG:4674")
  area      <- terra::project(area,      "EPSG:4674")
  dqf       <- terra::project(dqf,       "EPSG:4674")
  
  if(exists('bcm_clear_sky_crop')){
    bcm_clear_sky_crop <- terra::project(bcm_clear_sky_crop, "EPSG:4674")
    bcm_clear_sky_crop <- get_val_coor(bcm_clear_sky_crop) |> 
      dplyr::mutate(var = 'bcm_clear_sky')
    
    dqf_clear_sky_crop <- terra::project(dqf_clear_sky_crop, "EPSG:4674")
    dqf_clear_sky_crop <- get_val_coor(dqf_clear_sky_crop) |> 
      dplyr::mutate(var = 'bcm_clear_sky')
  }
  
  # -------------------------------------------------------------------
  # Decodificar FireMask mantendo como raster
  fire_mask <- terra::app(fire_mask, fun = function(x) bitwAnd(x, 1))
  dqf <- terra::app(dqf, fun = function(x) bitwAnd(bitwShiftR(x, 2), 3))
  
  # -------------------------------------------------------------------
  # Extrair valores
  fogo <- get_val_coor(fire_mask) |> 
    dplyr::filter(value == 1) |>
    dplyr::mutate(var = 'fogo')
  
  if(nrow(fogo) == 0) return(NULL)
  
  temperatura <- get_val_coor(temp) |> 
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(var = 'temp', value = value - 273.15)
  
  radiativo <- get_val_coor(power) |> 
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(var = 'radiatv')
  
  qualidade <- get_val_coor(dqf) |> 
    dplyr::filter(!is.na(value)) |> 
    dplyr::mutate(var = 'qualidade')
  
  area <- get_val_coor(area) |> 
    dplyr::filter(!is.na(value)) |> 
    dplyr::mutate(var = 'area')
  
  # -------------------------------------------------------------------
  # Organizar dados
  res <- bind_rows(fogo, temperatura, radiativo, qualidade, area)
  
  if(exists('bcm_clear_sky_crop')){
    res <- bind_rows(res, bcm_clear_sky_crop, dqf_clear_sky)
  }
  
  res <- res |> 
    tibble::as_tibble() |> 
    tidyr::pivot_wider(names_from = var, values_from = value) |> 
    dplyr::mutate(arquivo = str_extract(caminho_fogo$file, "[^/]+$")) |> 
    dplyr::mutate(aux = str_sub(arquivo, 40, -20)) |> 
    dplyr::mutate(ano  = str_sub(aux, 1, 4),
                  doy  = str_sub(aux, 5, 7), 
                  hora = str_sub(aux, 8, 9),
                  min  = str_sub(aux, 10, 11),
                  seg  = str_sub(aux, 12, 13)) |> 
    dplyr::select(-aux) |> 
    dplyr::mutate_at(vars(ano, hora, min, seg), as.numeric) |> 
    dplyr::mutate(dt = make_date(year = ano, month = 1, day = 1) + days(as.numeric(doy) - 1),
                  dt_hms = ymd_hms(paste0(dt, ' ', hora, ':', min, ':', seg)),
                  dia = day(dt_hms), 
                  mes = month(dt_hms)) |> 
    dplyr::select(-c(dt, ano, doy, hora, min, seg, dia, mes))
  
  return(res)
}


get_val_coor <- function(r){
    # r <- fire_mask
    coords <- xyFromCell(r, 1:ncell(r))
    vals <- values(r)
    
    tibble(x = coords[,1],
           y = coords[,2],
           value = as.vector(vals))
}

# safe_write <- function(data, final_path) {
#     temp_path <- paste0(paste0(stringr::str_remove(final_path, '.csv'), '_temp.csv'))
#     
#     # Tenta escrever no arquivo temporário
#     tryCatch({
#         readr::write_csv(data, temp_path)
#         file.rename(temp_path, final_path)  # Move apenas se a escrita foi bem-sucedida
#         message("Arquivo salvo com segurança em: ", final_path)
#     }, error = function(e) {
#         message("Erro ao salvar o arquivo: ", e$message)
#         if (file.exists(temp_path)) file.remove(temp_path)
#     })
# }

safe_write_parquet <- function(data, final_path) {
    temp_path <- paste0(stringr::str_remove(final_path, '\\.parquet$'), '_temp.parquet')
    
    tryCatch({
        arrow::write_parquet(data, temp_path)
        file.rename(temp_path, final_path)
        # message("Arquivo salvo com segurança: ", final_path)
    }, error = function(e) {
        message("Erro ao salvar: ", e$message)
        if (file.exists(temp_path)) file.remove(temp_path)
    })
}

to_shape <- function(df_caminhos, shape){
    mclapply(df_caminhos$file, down_goes, mc.cores = 6)
    
    df_caminhos <- df_caminhos |> 
      mutate(file=paste0('files/', basename(file)))
      
    fogo <- df_caminhos |> 
      filter(str_detect(file, 'FDCF'))
    
    # Abrir variáveis
    fire_mask <- terra::rast(fogo$file, subds = "Mask")
    temp      <- terra::rast(fogo$file, subds = "Temp")
    power     <- terra::rast(fogo$file, subds = "Power")
    area      <- terra::rast(fogo$file, subds = "Area")
    dqf       <- terra::rast(fogo$file, subds = "DQF")
    
    fire_mask <- crop_2_epsg(fire_mask, shape)
    
    # Decodificar FireMask mantendo como raster
    fire_mask <- terra::app(fire_mask, fun = function(x) bitwAnd(x, 1))
    
    dqf <- crop_2_epsg(dqf, shape)
    dqf <- terra::app(dqf, fun = function(x) bitwAnd(bitwShiftR(x, 2), 3))
    
    temp <- crop_2_epsg(temp, shape)
    power <- crop_2_epsg(power, shape)
    area <- crop_2_epsg(area, shape)
    
    fogo <- get_val_coor(fire_mask) |> 
        dplyr::filter(value==1) |>
        dplyr::mutate(var='fogo')
    
    if(nrow(fogo)==0) return(NULL)
    
    temperatura <- get_val_coor(temp) |> 
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(var='temp') |> 
        dplyr::mutate(value=value - 273.15)
    
    radiativo <- get_val_coor(power) |> 
        dplyr::filter(!is.na(value)) |>
        dplyr::mutate(var='radiatv')
    
    qualidade <- get_val_coor(dqf) |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::mutate(var='qualidade')
    
    area <- get_val_coor(area) |> 
        dplyr::filter(!is.na(value)) |> 
        dplyr::mutate(var='area')
    
    # -------------------------------------------------------------------------
    
    res <- bind_rows(fogo, temperatura, radiativo, qualidade, area) |> 
        tibble::as_tibble() |> 
        tidyr::pivot_wider(names_from = var, values_from = value) |> 
        dplyr::mutate(arquivo=str_extract(nc_path, "[^/]+$")) |> 
        dplyr::mutate(aux=str_sub(arquivo, 40, -20)) |> 
        dplyr::mutate(ano=str_sub(aux, 1, 4), doy=str_sub(aux, 5, 7), 
               hora=str_sub(aux, 8, 9), min=str_sub(aux, 10, 11),
               seg=str_sub(aux, 12, 13)) |> 
        dplyr::select(-aux) |> 
        dplyr::mutate_at(vars(ano, hora, min, seg), as.numeric) |> 
        dplyr::mutate(dt=make_date(year = ano, month = 1, day = 1) + days(as.numeric(doy) - 1)) |> 
        dplyr::mutate(dt_hms=ymd_hms(paste0(dt, ' ', hora, ':', min, ':', seg))) |> 
        dplyr::mutate(dia=day(dt_hms), mes=month(dt_hms)) |> 
        dplyr::select(-dt)
        
        # res_list[[length(res_list) + 1]] <- res
        
        # dir.create(paste0('dados_extraidos/', caminho$ano, '/', caminho$dia, '/', caminho$hora), 
        #            recursive = TRUE, showWarnings = FALSE)
        # 
        # safe_write_parquet(res,
        #                    paste0('dados_extraidos/', caminho$ano, '/', caminho$dia, '/', 
        #                           caminho$hora, '/', stringr::str_remove(caminho$arquivo, '.nc'), '.parquet'))
    # }
    
    # res_final <- bind_rows(res_list)
    return(res)
}

to_shape2 <- function(nc_path, shape){
    return(tryCatch(to_shape(nc_path, shape), error=function(e) NULL))
}

proc_ano <- function(data0, data1, shape, canais){
    data0 <- '2020-01-01'
    data1 <- '2020-01-01'
    shape <- pantanal
    canais <- 3
    
# -------------------------------------------------------------------------
    canais <- as.vector(canais)
    canais <- str_pad(canais, 2, 'left', '0')
    shape <- vect(shape)
    system('rm -rf files/*')
    
    data_seq <- tibble(data=seq(ymd(data0), ymd(data1), by='days')) |> 
      mutate(doy=lubridate::yday(data),
             doy=str_pad(doy, width = 3, side = 'left', pad = '0'),
             ano=year(data))
    
    for(dia0 in data_seq$doy){
        system(paste0('aws s3 sync --no-sign-request --no-progress s3://noaa-goes16/ABI-L2-FDCF/', 
                      data_seq$ano, '/', 
                      data_seq$doy, ' files'))
        
        for(canal in canais){
          system(paste0('aws s3 cp --no-sign-request --no-progress --recursive ',
                        's3://noaa-goes16/ABI-L2-CMIPF/',
                        data_seq$ano, '/', 
                        data_seq$doy, '/ ',
                        'files/',
                        ' --exclude "*" --include "*M6C', canal, '*"'))
        }
        
        caminhos <- list.files('files', pattern = '.nc', full.names = T, recursive = T)
        # 
        # res_final <- parallel::mclapply(
        #     caminhos, function(caminhos){
        #         to_shape2(caminhos, shape)
        #         }, 
        #     mc.cores = detectCores()-3)
        # 
        # res_final <- dplyr::bind_rows(res_final)
        # 
        # arrow::write_dataset(res_final,
        #                      path = paste0('dados_extraidos/'),
        #                      format = "parquet",
        #                      partitioning = c('ano', 'mes', 'dia', 'hora'),
        #                      existing_data_behavior = "delete_matching")
        
        # system('rm -rf files/*')
    }
}


library(tibble)
library(dplyr)
library(stringr)

get_filename <- function(x){
  # separa por espaços em branco (ou outro padrão)
  parts <- str_split(x, "\\s+")[[1]]
  
  # pega o último elemento (que contém o caminho começando com "ABI-L2-FDCF")
  last_part <- parts[length(parts)]
  
  return(as.character(last_part))
}

cria_caminhos <- function(produto, canais=NULL, ano, doy){
  # produto <- "ABI-L2-FDCF"
  # doy=data_seq$doy[1]
  # ano=data_seq$ano[1]
  # canais=3
  
  # Caminho base S3
  base_path <- paste0("s3://noaa-goes16/", produto, '/', ano, "/", doy, "/")
  
  # Lista todos os arquivos no caminho especificado
  arquivos <- system(
    paste("aws s3 ls", base_path, "--recursive --no-sign-request"),
    intern = TRUE
  )
  
  arquivos <- sapply(arquivos, get_filename, USE.NAMES = F)

  if(!is.null(canais)){
    # Extrai o nome do arquivo
    tibble(file=arquivos) |> 
      mutate(timestamp=str_extract(file, "s\\d{14}") |> str_remove("^s"),
             canal=str_replace(str_extract(file, "M6C\\d{2}"), "M6C", ""),
             canal=as.numeric(canal)) |> 
      filter(canal%in%as.vector(canais))
  }else{
    # Extrai o nome do arquivo
    tibble(file=arquivos) |> 
      mutate(timestamp=str_extract(file, "s\\d{14}") |> str_remove("^s"))
  }
}





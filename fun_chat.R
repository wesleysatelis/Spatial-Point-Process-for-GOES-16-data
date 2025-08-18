# ---- auxiliares (mesmos nomes) ----
extract_ch <- function(caminho_ch, shape_proj, fire_mask){
  out <- lapply(caminho_ch, function(f){
    
    # tenta abrir CMI ou Rad
    cmi <- try(terra::rast(f, subds = "CMI"), silent = TRUE)
    if (inherits(cmi, "try-error")) {
      cmi <- try(terra::rast(f, subds = "Rad"), silent = TRUE)
      base_name <- "rad"
    } else {
      base_name <- "cmi"
    }
    if (inherits(cmi, "try-error")) return(NULL)
    
    # extrair número da banda (ex: "03", "06", "13")
    band_num <- stringr::str_extract(basename(f), "C(\\d{2})")
    band_num <- gsub("C", "", band_num)  # tira o "C"
    
    # nome final da variável
    cmi_name <- paste0(base_name, "_", band_num)
    
    # reprojeta e ajusta
    cmi <- terra::mask(terra::crop(cmi, shape_proj), shape_proj)
    cmi <- terra::resample(cmi, fire_mask, method = "bilinear")
    cmi <- terra::project(cmi, "EPSG:4674")
    
    # DQF do CMIPF = categórico -> near (se existir)
    dqf <- try(terra::rast(f, subds = "DQF"), silent = TRUE)
    if (!inherits(dqf, "try-error")) {
      dqf <- terra::mask(terra::crop(dqf, shape_proj), shape_proj)
      dqf <- terra::resample(dqf, fire_mask, method = "near")
      dqf <- terra::project(dqf, "EPSG:4674")
    } else {
      dqf <- NULL
    }
    
    # extrai (formato long: x,y,value,var) + coluna file
    tb <- get_val_coor_ch(cmi, dqf, cmi_name)
    return(tb)
  })
  
  out <- out[!sapply(out, is.null)]
  if (length(out) == 0) return(NULL)
  
  dplyr::bind_rows(out)
}


get_val_coor_ch <- function(rast_cmi, rast_dqf = NULL, cmi_name = "cmi"){
  coords <- xyFromCell(rast_cmi, 1:ncell(rast_cmi))
  vals_cmi <- values(rast_cmi)
  res <- tibble::tibble(
    x = coords[,1],
    y = coords[,2],
    value = as.vector(vals_cmi),
    var = cmi_name
  ) |> dplyr::filter(!is.na(value))
  
  if (!is.null(rast_dqf)) {
    vals_dqf <- values(rast_dqf)
    res_dqf <- tibble::tibble(
      x = coords[,1],
      y = coords[,2],
      value = as.vector(vals_dqf),
      var = "dqf_cmi"
    ) |> dplyr::filter(!is.na(value))
    res <- dplyr::bind_rows(res, res_dqf)
  }
  res
}

get_val_coor <- function(r){
  coords <- xyFromCell(r, 1:ncell(r))
  vals <- values(r)
  tibble::tibble(x = coords[,1], y = coords[,2], value = as.vector(vals))
}

to_shape <- function(df_caminhos, shape) {
  # 0) baixa os arquivos (igual)
  lapply(df_caminhos$file, down_goes)
  
  # 1) normaliza caminhos locais (igual)
  df_caminhos <- df_caminhos |> 
    mutate(file = paste0('files/', basename(file)))
  
  caminho_fogo <- df_caminhos |> filter(produto=='fdcf')
  caminho_clear_sky <- df_caminhos |> filter(produto=='acmc')
  
  # 2) abre FDCF (igual)
  fire_mask <- terra::rast(caminho_fogo$file, subds = "Mask")
  temp      <- terra::rast(caminho_fogo$file, subds = "Temp")
  power     <- terra::rast(caminho_fogo$file, subds = "Power")
  area      <- terra::rast(caminho_fogo$file, subds = "Area")
  dqf       <- terra::rast(caminho_fogo$file, subds = "DQF")
  
  # 3) crop+mask no CRS nativo do FDCF (igual)
  shape_proj <- terra::project(shape, terra::crs(fire_mask))
  fire_mask <- terra::mask(terra::crop(fire_mask, shape_proj), shape_proj)
  temp      <- terra::mask(terra::crop(temp,      shape_proj), shape_proj)
  power     <- terra::mask(terra::crop(power,     shape_proj), shape_proj)
  area      <- terra::mask(terra::crop(area,      shape_proj), shape_proj)
  dqf       <- terra::mask(terra::crop(dqf,       shape_proj), shape_proj)
  
  # 4) tenta ler 1 arquivo válido do ACMC (BCM e DQF) e já fazer crop+mask
  #    (mantém nomes bcm_clear_sky_crop e dqf_clear_sky_crop)
  if (nrow(caminho_clear_sky) > 0) {
    for (i in seq_along(caminho_clear_sky$file)) {
      try({
        # ---------------------------------------------------
        # DQF
        dqf_clear_sky <- terra::rast(caminho_clear_sky$file[i], subds = "DQF")
        
        if (terra::relate(ext(dqf_clear_sky), ext(shape_proj), relation = "intersects")) {
          dqf_clear_sky_crop <- terra::mask(terra::crop(dqf_clear_sky, shape_proj), shape_proj)
        } else {
          message("⚠️ Sem overlay entre DQF Clear Sky e shape para o arquivo: ", caminho_clear_sky$file[i])
        }
        
        # ---------------------------------------------------
        # BCM
        bcm_clear_sky <- terra::rast(caminho_clear_sky$file[i], subds = "BCM")
        
        if (terra::relate(ext(bcm_clear_sky), ext(shape_proj), relation = "intersects")) {
          bcm_clear_sky_crop <- terra::mask(terra::crop(bcm_clear_sky, shape_proj), shape_proj)
        } else {
          message("⚠️ Sem overlay entre BCM Clear Sky e shape para o arquivo: ", caminho_clear_sky$file[i])
        }
      })
    }
  }
  
  # 5) resample para a grade do fire_mask (2 km)
  temp  <- terra::resample(temp,  fire_mask, method = "bilinear") # contínuas
  power <- terra::resample(power, fire_mask, method = "bilinear")
  area  <- terra::resample(area,  fire_mask, method = "bilinear")
  dqf   <- terra::resample(dqf,   fire_mask, method = "near")     # categórica
  
  # 7) CMIPF: extrai por canal mantendo sua lógica
  canais <- df_caminhos |> 
    filter(produto=='cmipf')
  
  canais_final <- extract_ch(canais$file, shape_proj, fire_mask)
  
  # 6) reprojeta tudo para EPSG:4674 (mantendo sua ordem)
  fire_mask <- terra::project(fire_mask, "EPSG:4674")
  temp      <- terra::project(temp,      "EPSG:4674")
  power     <- terra::project(power,     "EPSG:4674")
  area      <- terra::project(area,      "EPSG:4674")
  dqf       <- terra::project(dqf,       "EPSG:4674")
  
  # 8) se conseguiu ler ACMC, projeta e transforma em long (valor/var)
  if (exists('bcm_clear_sky_crop')) {
    bcm_clear_sky_crop <- terra::project(bcm_clear_sky_crop, "EPSG:4674")
    bcm_clear_sky_crop <- get_val_coor(bcm_clear_sky_crop) |> 
      dplyr::mutate(var = 'bcm_clear_sky')
  }
  if (exists('dqf_clear_sky_crop')) {
    dqf_clear_sky_crop <- terra::project(dqf_clear_sky_crop, "EPSG:4674")
    dqf_clear_sky_crop <- get_val_coor(dqf_clear_sky_crop) |> 
      dplyr::mutate(var = 'dqf_clear_sky')   # <- conserta o rótulo
  }
  
  # 9) decodifica FireMask e DQF do FDCF (igual)
  fire_mask <- terra::app(fire_mask, fun = function(x) bitwAnd(x, 1))
  dqf       <- terra::app(dqf,       fun = function(x) bitwAnd(bitwShiftR(x, 2), 3))
  
  # 10) extrai valores (igual, com pequenas seguranças)
  fogo <- get_val_coor(fire_mask) |> 
    dplyr::filter(value == 1) |>
    dplyr::mutate(var = 'fire')
  if (nrow(fogo) == 0) return(NULL)
  
  temperatura <- get_val_coor(temp) |> 
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(var = 'temp', value = value - 273.15)
  
  radiativo <- get_val_coor(power) |> 
    dplyr::filter(!is.na(value)) |>
    dplyr::mutate(var = 'radiatv')
  
  qualidade <- get_val_coor(dqf) |> 
    dplyr::filter(!is.na(value)) |> 
    dplyr::mutate(var = 'dqf_fire')
  
  area <- get_val_coor(area) |> 
    dplyr::filter(!is.na(value)) |> 
    dplyr::mutate(var = 'area')
  
  # 11) organiza dados e agrega clear sky + canais (formatos compatíveis)
  res <- dplyr::bind_rows(fogo, temperatura, radiativo, qualidade, area, canais_final)
  if (exists('bcm_clear_sky_crop')) res <- dplyr::bind_rows(res, bcm_clear_sky_crop)
  if (exists('dqf_clear_sky_crop')) res <- dplyr::bind_rows(res, dqf_clear_sky_crop)
  
  res <- res |> 
    tibble::as_tibble() |> 
    dplyr::mutate(dt_hms = lubridate::ymd_hms(unique(df_caminhos$start_time)),
                  day=day(dt_hms), year=year(dt_hms), month=month(dt_hms), hour=hour(dt_hms))
  
  arrow::write_dataset(res, 'dados_extraidos', format = "parquet", 
                       partitioning=c('var', 'year', 'month', 'day', 'hour'), existing_data_behavior = 'delete_matching')
  
  file.remove(df_caminhos$file)
  
  return(NULL)
}







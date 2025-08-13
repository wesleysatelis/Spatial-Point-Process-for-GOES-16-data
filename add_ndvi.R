rm(list = ls())
library(arrow)
library(patchwork)
library(lubridate)
library(ggplot2)
source('fun.R', encoding = 'utf-8')
library(dplyr)

data <- open_dataset('dados_fogo//') |> 
    filter(fogo == 1 & temp > 47 & radiatv > 5 & ano==2020 & mes%in%c(8)) |> 
    collect()

ano0 <- unique(data$ano)[1]

# bandas 3, 2, e 6
pantanal <- st_read('pantanal_shape/pantanal.shp')
pantanal <- vect(pantanal)

# to_pantanal_clear_sky <- function(nc_path, pantanal){
#     # nc_path <- 'files/00/OR_ABI-L2-ACMF-M6_G16_s20202140000189_e20202140009497_c20202140010276.nc'
#     
#     clear_sky_dqf <- terra::rast(nc_path, subds='DQF')
#     clear_sky_dqf <- crop_2_epsg(clear_sky_dqf, pantanal)
#     clear_sky_dqf <- terra::app(clear_sky_dqf, fun = function(x) x == 0)
#     dqf_final <- get_val_coor(clear_sky_dqf) |> 
#         filter(!is.na(value)) |> 
#         mutate(var='dqf')
#     
#     clear_sky_bcm <- terra::rast(nc_path, subds='BCM')
#     clear_sky_bcm <- crop_2_epsg(clear_sky_bcm, pantanal)
#     clear_sky_bcm <- terra::app(clear_sky_bcm, fun = function(x) bitwAnd(x, 3))
#     
#     bcm_final <- get_val_coor(clear_sky_bcm) |> 
#         filter(value==0) |> 
#         mutate(var='bcm')
#     
#     res <- bind_rows(bcm_final, dqf_final) |> 
#         tidyr::pivot_wider(names_from = var, values_from = value) |> 
#         dplyr::mutate(arquivo=str_extract(nc_path, "[^/]+$")) |> 
#         dplyr::mutate(aux=str_sub(arquivo, 40, -20)) |> 
#         dplyr::mutate(ano=str_sub(aux, 1, 4), doy=str_sub(aux, 5, 7), 
#                       hora=str_sub(aux, 8, 9), min=str_sub(aux, 10, 11),
#                       seg=str_sub(aux, 12, 13)) |> 
#         dplyr::select(-aux) |> 
#         dplyr::mutate_at(vars(ano, hora, min, seg), as.numeric) |> 
#         dplyr::mutate(dt=make_date(year = ano, month = 1, day = 1) + days(as.numeric(doy) - 1)) |> 
#         dplyr::mutate(dt_hms=ymd_hms(paste0(dt, ' ', hora, ':', min, ':', seg))) |> 
#         dplyr::mutate(dia=day(dt_hms), mes=month(dt_hms), data_type='clear_sky') |> 
#         dplyr::select(-dt)
#     
#     return(res)
# }
# for(doi0 in unique(data$doy)){
#     # doi0 <- unique(data$doy)[2]
#     system(paste0('aws s3 sync --no-sign-request --no-progress s3://noaa-goes16/ABI-L2-ACMF/', ano0, '/', 
#                   doi0, ' files'))
#     
#     caminhos <- list.files('files', pattern = '.nc', full.names = T, recursive = T)
#     
#     res_final <- parallel::mclapply(
#         caminhos, function(caminhos){
#             to_pantanal_clear_sky(caminhos, pantanal = pantanal)
#         }, 
#         mc.cores = 6)
#     
#     res_final <- dplyr::bind_rows(res_final)
#     
#     arrow::write_dataset(res_final,
#                          path = paste0('dados_extraidos/'),
#                          format = "parquet",
#                          partitioning = c('data_type', 'ano', 'mes', 'dia', 'hora'),
#                          existing_data_behavior = "delete_matching")
#     
#     system('rm -rf files/*')
#     # gc()
# }

to_pantanal_canais <- function(nc_path, pantanal){
    # nc_path <- arquivos_aws$value[1]
    # terra::rast(nc_path)
    
    system(paste0('aws s3 cp --no-sign-request --no-progress s3://noaa-goes16/', nc_path,  ' files'))
    
    nc_path <- paste0('files/', str_extract(nc_path, "[^/]+$"))
    
    rad <- terra::rast(nc_path, subds='Rad')
    rad <- crop_2_epsg(rad, pantanal)
    
    dqf <- terra::rast(nc_path, subds='DQF')
    dqf <- crop_2_epsg(dqf, pantanal)
    
    dqf_final <- get_val_coor(dqf) |> 
        filter(!is.na(value)) |> 
        mutate(var='dqf')
    
    rad_final <- get_val_coor(rad) |> 
        filter(!is.na(value)) |> 
        mutate(var='rad')
    
    res <- bind_rows(dqf_final, rad_final) |> 
        tidyr::pivot_wider(names_from = var, values_from = value) |> 
        dplyr::mutate(arquivo=str_extract(nc_path, "[^/]+$")) |> 
        dplyr::mutate(aux=str_sub(arquivo, 44, -20), canal=str_sub(arquivo, 19, 21)) |> 
        dplyr::mutate(ano=str_sub(aux, 1, 4), doy=str_sub(aux, 5, 7), 
                      hora=str_sub(aux, 8, 9), min=str_sub(aux, 10, 11),
                      seg=str_sub(aux, 12, 13)) |> 
        dplyr::select(-aux) |> 
        dplyr::mutate_at(vars(ano, hora, min, seg), as.numeric) |> 
        dplyr::mutate(dt=make_date(year = ano, month = 1, day = 1) + days(as.numeric(doy) - 1)) |> 
        dplyr::mutate(dt_hms=ymd_hms(paste0(dt, ' ', hora, ':', min, ':', seg))) |> 
        dplyr::mutate(dia=day(dt_hms), mes=month(dt_hms)) |> 
        dplyr::select(-dt)
    
    file.remove(nc_path)
    
    return(res)
}

for(doi0 in unique(data$doy)){
    # doi0 <- unique(data$doy)[1]
    
    arquivos_aws <- system(paste0('aws s3 ls --recursive --no-sign-request s3://noaa-goes16/ABI-L1b-RadF/', ano0, '/', doi0, '/'), intern = TRUE) |> 
        as_tibble() |> 
        mutate(banda=str_extract(value, "[^/]+$")) |> 
        mutate(banda=str_sub(banda, 20, 21)) |> 
        mutate(banda=as.numeric(banda)) |> 
        filter(banda%in%c(2, 3, 6, 14)) |> 
        mutate(value=word(value, -1))
    
    caminhos <- arquivos_aws$value
    
    res_final <- parallel::mclapply(
        caminhos, function(caminhos){
            to_pantanal_canais(caminhos, pantanal = pantanal)
            }, mc.cores = 6)
    
    res_final <- dplyr::bind_rows(res_final)
    
    arrow::write_dataset(res_final,
                         path = paste0('dados_extraidos/'),
                         format = "parquet",
                         partitioning = c('canal', 'ano', 'mes', 'dia', 'hora'),
                         existing_data_behavior = "delete_matching")
    
    system('rm -rf files/*')
    # gc()
}


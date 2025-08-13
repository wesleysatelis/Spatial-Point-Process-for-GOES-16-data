rm(list=ls())
library(arrow)
library(tidyverse)

fogo <- open_dataset('dados_extraidos/fogo/') |> 
    filter(fogo == 1 & temp > 47 & radiatv > 5 & 
               ano==2020 & mes%in%c(8) & dia==1 & hora==10) |> 
    collect() |> 
    select(-c(min, seg, ano, mes, dia, hora, arquivo)) |> 
    rename(x_fogo=y, y_fogo=x)

clear_sky <- open_dataset('dados_extraidos/clear_sky/') |> 
    filter(dia==1, hora==10) |> 
    collect() |> 
    select(-c(min, seg, ano, mes, dia, hora, arquivo))

canais <- open_dataset('dados_extraidos/canais/') |> 
    filter(dia==1, hora==15) |> 
    collect()

canais |> 
    count(dt_hms, canal) |> 
    group_by(dt_hms) |> 
    mutate(n_canais=n()) |> 
    ungroup() |> 
    filter(n_canais==max(n_canais))

canais2 <- canais |> 
    filter(dt_hms=='2020-08-01 15:59:49') |>
    # rename(x_canais=y, y_canais=x) |> 
    select(-c(arquivo, dqf, min, seg, ano, mes, dia, hora)) |> 
    left_join(clear_sky)

# 0 = céu limpo
# 1 = provavelmente limpo
# 2 = provavelmente nublado
# 3 = nublado

horas <- distinct(canais2, dt_hms)
pantanal <- st_read('pantanal_shape/pantanal.shp')

canais2 |>
    ggplot() +
    # geom_sf(data = pantanal, fill = NA, color = "black") +
    geom_raster(aes(x = x, y = y, fill = rad)) + 
    scale_fill_viridis_c(aes(x = x, y = y, fill = rad), 
                         option = "D", na.value = "transparent") +
    coord_equal() +
    labs(title = "Canal 2 GOES-16",
        fill = "Rad") + 
    facet_wrap(~canal)

# library(terra)
# 
# b2 <- rast("C02.nc")  # 0.5 km
# b3 <- rast("C03.nc")  # 1.0 km
# b6 <- rast("C06.nc")  # 2.0 km
# 
# B3 e B6 para resolução e grade da B2
# b3_resampled <- resample(b3, b2, method = "bilinear")  # ou "near" para classificações
# b6_resampled <- resample(b6, b2, method = "bilinear")
# 
# stacked <- c(b2, b3_resampled, b6_resampled)
# 
# names(stacked) <- c("B2", "B3", "B6")
# valores <- as.data.frame(stacked, xy = TRUE, na.rm = FALSE)




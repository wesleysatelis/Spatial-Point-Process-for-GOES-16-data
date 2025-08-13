rm(list = ls())
library(arrow)
library(patchwork)
library(lubridate)
library(ggplot2)
source('fun.R', encoding = 'utf-8')
library(dplyr)
gc()

data <- open_dataset('dados_extraidos/') |> 
    filter(fogo == 1 & temp > 47 & radiatv > 5 & ano==2020 & mes%in%c(8)) |> 
    collect()

data <- data |> 
    dplyr::select(-c(arquivo, doy, min, seg, ano, mes, dia, hora)) |> 
    # mutate(x=as.character(x), y=as.character(y)) |> 
    arrange(dt_hms)

data <- data |> 
    mutate(semana=week(dt_hms)) |> 
    filter(semana==32)

# existem obs do mesmo ponto em que a temp e radiativ mudam, então
# não podemos usar o acumulado diretamente pois haveriam pontos sobrepostos

data |> 
    count(dt_hms) |> 
    filter(n!=1)

# -------------------------------------------------------------------------
# # converter tempo para formato numérico (ex: dias desde o primeiro evento)
# dados_fogo <- data |>
#     mutate(t = as.numeric(difftime(dt_hms, min(dt_hms), units = "hours")))
# 
# # criar janela espacial (ajuste os limites conforme seus dados)
# janela <- owin(xrange = range(dados_fogo$x),
#                yrange = range(dados_fogo$y))
# 
# # objeto de pontos espaciais
# pontos_espaciais <- ppp(x = dados_fogo$x,
#                         y = dados_fogo$y,
#                         window = janela)
# 
# # adicionar a coordenada temporal
# dados_st <- as.ppp(pontos_espaciais)
# marks(dados_st) <- dados_fogo$t
# 
# # estimar intensidade espacial com kernel
# lambda_spatial <- density(dados_st, sigma = bw.ppl(dados_st))
# lambda_df <- as_tibble(lambda_spatial)
# 
# pantanal <- st_read('pantanal_shape/pantanal.shp')
# pontos_sf <- st_as_sf(data, coords = c("x", "y"), crs = st_crs(pantanal))
# 
# lambda_sf <- st_as_sf(lambda_df, coords = c("x", "y"), crs = st_crs(pantanal))
# lambda_sf <- st_intersection(lambda_sf, pantanal)
# 
# p1 <- lambda_df |>
#     ggplot() +
#     geom_sf(data = lambda_sf, aes(color=value), alpha = 1, size=1.5) +
#     scale_color_viridis_c(option = "inferno",
#                           oob = scales::squish, direction = -1) +
#     geom_sf(data = pantanal, fill = NA, color = "black") +
#     ggtitle('Estimativa de intensidade espacial no mes 8.')
# 
# p2 <- data |>
#     mutate(mes=month(dt_hms)) |> 
#     ggplot() +
#     geom_sf(data = pantanal, fill = "white", color = "black") +
#     geom_sf(data = pontos_sf, alpha = 0.05, size=1.5) +
#     scale_color_viridis_c(option = "inferno", limits = c(0, 300),
#                           oob = scales::squish, direction = -1) +
#     ggtitle('Frequência de incêndios no mes 8.')
# 
# p1 + p2
# 
# # plot(lambda_spatial, main = "Intensidade espacial estimada")
# # points(dados_st, col = "white", pch = 16, cex = 0.3)


# metodo da rejeição ------------------------------------------------------

# extrair hora do dia (0 a 23) da coluna dt_hms
# dt <- data %>%
#     mutate(hora = hour(dt_hms) + minute(dt_hms)/60 + second(dt_hms)/3600)
# 
# pantanal <- st_read('pantanal_shape/pantanal.shp')
# pontos_sf <- st_as_sf(data, coords = c("x", "y"), crs = st_crs(pantanal))
# 
# # contar quantos eventos por hora
# lambda_empirica <- dt %>%
#     count(hora = floor(hora)) %>%
#     mutate(lambda = n / sum(n) * 24)  # normalizar para taxa por hora
# 
# # visualizar a intensidade temporal estimada
# ggplot(lambda_empirica, aes(x = hora, y = lambda)) +
#     geom_col(fill = "orange") +
#     geom_smooth(method = "loess", se = FALSE, color = "blue", span = 0.3) +
#     labs(title = "Estimativa empírica de λ(t)",
#          x = "Hora do dia", y = "Intensidade (eventos esperados por hora)") +
#     theme_minimal()





rm(list = ls())
library(arrow)
library(patchwork)
library(lubridate)
library(ggplot2)
source('0_fun.R', encoding = 'utf-8')
library(dplyr)

data <- open_dataset('dados_extraidos/') |> 
    collect()

# fire == 1 & temp > 47 & radiatv > 5 & ano==2020

data_fire <- data |> 
    filter(!str_detect(var, 'cmi')) |> 
    pivot_wider(names_from = var, values_from = value) |> 
    filter(fire==1, temp > 47 & radiatv > 5)

data_fire <- data_fire |> 
    select(x, y, dt_hms, fire)

# tem que pegar dqf_cmi_03!
# canais |> 
#     filter(var=='dqf_cmi') |> 
#     distinct()

canais <- data |> 
    filter(str_detect(var, 'cmi')) |> 
    filter(var!='dqf_cmi') |> 
    pivot_wider(names_from = var, values_from = value) |> 
    mutate(nbr=(cmi_03 - cmi_07) / (cmi_03 + cmi_07),
           ndvi=(cmi_03 - cmi_02) / (cmi_03 + cmi_02)) |> 
    select(x, y, dt_hms, nbr, ndvi)

canais <- canais |> 
    group_by(x, y) |> 
    drop_na() |> 
    reframe(nbr=max(nbr, na.rm = T),
            ndvi=max(ndvi, na.rm = T))


data_fire
canais

# plots -------------------------------------------------------------------
shape <- st_read('pantanal_shape/pantanal.shp')

# transformar o tibble em sf
df_sf <- st_as_sf(canais, coords = c("x", "y"), crs = 4674)  # ou crs do seu dado
# transformar focos de fogo em sf
df_fire_sf <- st_as_sf(data_fire, coords = c("x", "y"), crs = 4674)

# plotar NDVI + pontos de fogo
ggplot() +
    geom_sf(data = shape, fill = NA, color = "black") +
    geom_sf(data = df_sf, aes(color = ndvi), size = 0.4) +
    scale_color_viridis_c(option = "D") +
    geom_sf(data = df_fire_sf, color = "white", size = 1, shape = 3) + # X vermelho
    theme_minimal() +
    ggtitle("NDVI + focos de fogo")

# plotar NBR + pontos de fogo
ggplot() +
    geom_sf(data = shape, fill = NA, color = "black") +
    geom_sf(data = df_sf, aes(color = ndvi), size = 0.4) +
    scale_color_viridis_c(option = "C") +
    geom_sf(data = df_fire_sf, color = "black", size = 1, shape = 3) + # X vermelho
    theme_minimal() +
    ggtitle("NBR + focos de fogo")


# Baddeley, verossimilhança de um processo de poisson, covariável é usada na função de intensidade
# 4.3 pseudolikelyhood
# como Diggle define a condicional da funcao intensidade para o caso de atração?
# olhar kppm do spatstat, usar shape no pacote e transformar cov em imagem, lgcp?
# mapbiomas com o uso da terra



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





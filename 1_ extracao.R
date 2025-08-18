rm(list=ls())
source('0_fun.R', encoding = 'utf-8')
source('fun_chat.R')
library(fuzzyjoin)

shape <- st_read('pantanal_shape/pantanal.shp')
shape <- vect(shape)

data0 <- '2020-09-01'
data1 <- '2020-09-07'
canais <- c(2, 3, 7)
# -------------------------------------------------------------------------
system('rm -rf files/*')

data_seq <- tibble(data=seq(ymd(data0), ymd(data1), by='days')) |> 
  mutate(doy=lubridate::yday(data),
         doy=str_pad(doy, width = 3, side = 'left', pad = '0'),
         ano=year(data))

caminhos <- list()
for(i in 1:nrow(data_seq)){
  temp <- data_seq[i,]
  
  arquivos_fogo <- cria_caminhos("ABI-L2-FDCF", doy=temp$doy, ano=temp$ano)
  arquivos_canais <- cria_caminhos("ABI-L2-CMIPF", canais, doy=temp$doy, ano=temp$ano) |> 
    select(-canal)
  arquivos_clear_sky <- cria_caminhos("ABI-L2-ACMC", doy=temp$doy, ano=temp$ano)
  
  caminhos[[length(caminhos)+1]] <- bind_rows(arquivos_fogo, arquivos_canais, arquivos_clear_sky)
}
caminhos <- bind_rows(caminhos)

df_caminhos <- caminhos |> 
  arrange(timestamp) |> 
  mutate(start_time = as.POSIXct(timestamp, format = "%Y%j%H%M%S", tz = "UTC")) |> 
  select(-timestamp) |> 
  mutate(produto = case_when(
    str_detect(file, "ABI-L2-FDCF") ~ "fdcf",
    str_detect(file, "ABI-L2-CMIPF") ~ "cmipf",
    str_detect(file, "ABI-L2-ACMC")  ~ "acmc",
    TRUE ~ "other"),
    canal=str_replace(str_extract(file, "M6C\\d{2}"), "M6C", ""),
    canal=as.numeric(canal))


# separar produtos
fdcf  <- df_caminhos |> 
  filter(produto == "fdcf") |>
  arrange(start_time) |>
  mutate(group_id = row_number())  # cada FDCF define um grupo

cmipf <- df_caminhos |> 
  filter(produto == "cmipf")
acmc  <- df_caminhos |> 
  filter(produto == "acmc")

# ---------------------------
# Juntar FDCF com CMIPF (mesmo start_time)
fdcf_cmipf <- fdcf |>
  left_join(cmipf, by = "start_time", suffix = c("_fdcf", "_cmipf")) |>
  mutate(group_id = group_id)  # mantém group_id do fdcf

# ---------------------------
# Juntar FDCF com ACMC (mais próximo no tempo, até 5 min de tolerância)
fdcf_acmc <- difference_left_join(fdcf, acmc, by = "start_time",
                                  max_dist = dminutes(5),
                                  distance_col = "time_diff") |>
  group_by(file.x) |>
  slice_min(time_diff, n = 1) |>   # pega ACMC mais próximo
  ungroup() |>
  transmute(file = file.y,
            start_time = start_time.y,
            produto = "acmc",
            canal = NA,
            group_id)

# ---------------------------
# Organizar estrutura final: tudo em linhas
df_final <- bind_rows(fdcf |> select(file, start_time, produto, canal, group_id),
                      cmipf |> inner_join(fdcf |> select(start_time, group_id), by = "start_time"),
                      fdcf_acmc) |>
  arrange(group_id, produto, canal)

df_final <- df_final |>
  group_by(group_id) |>
  mutate(start_time = as.POSIXct(names(sort(table(start_time), decreasing = TRUE)[1]), 
                                       tz = "UTC")) |>
  ungroup()

# -------------------------------------------------------------------------

df_list <- df_final |> 
  split(df_final$group_id)

mclapply(df_list, to_shape, shape, mc.cores = parallel::detectCores()-2)



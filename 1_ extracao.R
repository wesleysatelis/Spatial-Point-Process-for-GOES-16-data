rm(list=ls())
source('0_fun.R', encoding = 'utf-8')
source('fun_chat.R')

shape <- st_read('pantanal_shape/pantanal.shp')
shape <- vect(shape)

data0 <- '2020-06-01'
data1 <- '2020-12-31'
canais <- c(2, 3, 7)
# -------------------------------------------------------------------------
system('rm -rf files/*')

data_seq <- tibble(data=seq(ymd(data0), ymd(data1), by='days')) |> 
  mutate(doy=lubridate::yday(data),
         doy=str_pad(doy, width = 3, side = 'left', pad = '0'),
         ano=year(data))

arquivos_fogo <- cria_caminhos("ABI-L2-FDCF", doy=data_seq$doy[1], ano=data_seq$ano[1])
arquivos_canais <- cria_caminhos("ABI-L2-CMIPF", canais, doy=data_seq$doy[1], ano=data_seq$ano[1]) |> 
  select(-canal)
arquivos_clear_sky <- cria_caminhos("ABI-L2-ACMC", doy=data_seq$doy[1], ano=data_seq$ano[1])

df <- bind_rows(arquivos_fogo, arquivos_canais, arquivos_clear_sky) |> 
  arrange(timestamp) |> 
  mutate(start_time = as.POSIXct(timestamp, format = "%Y%j%H%M%S", tz = "UTC")) |> 
  select(-timestamp) |> 
  arrange(start_time) |> 
  mutate(diff_min = as.numeric(difftime(start_time, lag(start_time), units = "secs")),
         diff_min = ifelse(is.na(diff_min), 0, diff_min)) |> 
  mutate(group_id = cumsum(diff_min >= 300),
         group_id = ifelse(diff_min==300, group_id-1, group_id)) |> 
  group_by(group_id) |>
  mutate(n=n()) |> 
  ungroup()

df <- df |> 
  mutate(produto = case_when(
    str_detect(file, "ABI-L2-FDCF") ~ "fdcf",
    str_detect(file, "ABI-L2-CMIPF") ~ "cmipf",
    str_detect(file, "ABI-L2-ACMC")  ~ "acmc",
    TRUE ~ "other"),
    canal=str_replace(str_extract(file, "M6C\\d{2}"), "M6C", ""),
    canal=as.numeric(canal)) |> 
  arrange(group_id) |> 
  select(-c(diff_min, n)) |> 
  group_by(group_id, start_time) |> 
  # mutate(n=n()) |> 
  mutate(start_time=case_when(n()==1 ~ NA,
                              TRUE ~ start_time)) |> 
  group_by(group_id) |> 
  mutate(start_time=na.omit(unique(start_time))) |> 
  ungroup()

df_list <- df |> 
  split(df$group_id)

resultados <- mclapply(df_list, to_shape, shape, mc.cores = parallel::detectCores()-2)



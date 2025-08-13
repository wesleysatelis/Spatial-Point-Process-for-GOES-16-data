rm(list = ls())
library(arrow)
source('fun.R', encoding = 'utf-8')
gc()

data <- open_dataset('dados_extraidos/') |> 
    filter(fogo == 1 & temp > 47 & radiatv > 5 & ano==2020) |> 
    collect() 

# temperatura_range <- min(doys$temp, na.rm = T)
# 
# max(doys$temp, na.rm = T)

pantanal <- st_read('pantanal_shape/pantanal.shp')

for(i in as.character(unique(data$dt_hms))){
    # i <- unique(data$dt_hms)[1]
    i <- as.character(i)
    temp <- data |> 
        # filter(dia==i & hora>=10 & hora<=16, qualidade==0 & temp>47) |>
        filter(as.character(dt_hms)==i)

    # print(d)
    pontos_sf <- st_as_sf(temp, coords = c("x", "y"), crs = st_crs(pantanal))

    p <- temp |>
        ggplot() +
        geom_sf(data = pantanal, fill = "white", color = "black") +
        geom_sf(data = pontos_sf, aes(color=radiatv), alpha = 1, size=2) +
        scale_color_viridis_c(option = "inferno", limits = c(0, 300),
                              oob = scales::squish) +
        ggtitle(i)
    
    ggsave(filename = paste0('plots/2020/', i, '.png'), plot = p, width = 9, height = 12)
}



final |> 
    group_by(mes, dia) |> 
    summarise(n=n()) |> 
    ggplot(aes(x=dia, y=n)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~mes, ncol=3) + 
    ylab('Qtd. incendios')



pontos_sf <- st_as_sf(final, coords = c("x", "y"), crs = st_crs(pantanal))

final |>
    ggplot() +
    geom_sf(data = pantanal, fill = "white", color = "black") +
    geom_sf(data = pontos_sf, aes(color=radiatv), alpha = 0.3, size=1) +
    scale_color_viridis_c(option = "inferno", limits = c(0, 300),
                          oob = scales::squish) +
    ggtitle('Frequência de incêndios por mês.') + 
    facet_wrap(~mes)

final

# fazer analise do uso do solo IBGE, mapbiomas
# fazer processo pontual nao homogenio simples (processo de nascimento e morte)
# processo de atração (Cox) processo de inibição (Straus)




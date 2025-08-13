rm(list = ls())
source('fun.R', encoding = 'utf-8')
set.seed(3)

pantanal <- st_read('pantanal_shape/pantanal.shp')
coord <- pantanal$geometry
coord <- as_tibble(coord[[1]][[1]])
janela <- owin(poly = list(y=coord$V1, x=coord$V2))

# -------------------------------------------------------------------------
lambda = 1 # taxa de nascimento
mu <- 0.3 # taxa de morte
r <- 0.5 # raio
# -------------------------------------------------------------------------

pontos_aleat <- rpoispp(lambda = lambda, win=janela) |> 
    flipxy()

system('rm /home/rstudio/plots/simulacao/*')

for(i in 1:500){
    n <- pontos_aleat[["n"]]
    lambda_n <- lambda*n
    mu_n <- mu*n
    taxa_total <- lambda_n+mu_n
    
    if(runif(1) < lambda_n/taxa_total){
        # NASCIMENTO
        # gera um único ponto aleatório dentro do grid
        ponto_ppp <- runifpoint(1, win = janela)
        ponto_ppp <- flipxy(ponto_ppp)
        
        # verifica se existe algum ponto no raio r (definir propagação)
        dmin <- min(crossdist(ponto_ppp, pontos_aleat))
        
        if(dmin!=0){
            # probabilidade de aceitar (decrescente com a distância)
            prob <- exp(-dmin / r)
            if (runif(1) < prob) {
                pontos_aleat <- superimpose(pontos_aleat, ponto_ppp)
            }
        }
    }else{
        # MORTE
        pt_remove <- sample(1:n, 1)
        pontos_aleat <- pontos_aleat[-pt_remove]
    }
    
    pt_plot <- pontos_aleat |> 
        as_tibble() |> 
        st_as_sf(coords=c("x", "y"), crs=st_crs(pantanal))
    
    p <- ggplot() +
        geom_sf(data=pantanal, fill="white", color="black") +
        geom_sf(data=pt_plot, alpha=1, size=1.5)
    
    ggsave(filename = paste0('plots/simulacao/', i, '.png'), plot = p, width = 9, height = 12)
}

system("cd /home/rstudio/plots/simulacao/ && ffmpeg -framerate 4 -pattern_type glob -i '*.png' -c:v libx264 -pix_fmt yuv420p simulacao.mp4")

# plot(pontos, size=0.5)
# kest_pontos <- Kest(pontos)
# plot(kest_pontos)
# 
# st_area(pantanal)
# library(spatstat)
# library(MASS)

# codigo rmat3.R
# birth-death process for spatial data

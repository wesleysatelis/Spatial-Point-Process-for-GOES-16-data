set.seed(1)
files <- list.files('2020', full.names = T) |> 
    sample(10)
data <- rast(files[10])
pantanal <- st_read('pantanal_shape/pantanal.shp')

sub <- data[[c("Mask", "DQF")]]
values(sub[['Mask']]) <- round(as.vector(values(sub[['Mask']])), 0)
values(sub[['DQF']]) <- round(as.vector(values(sub[['DQF']])), 0)


qual <- 2
filtro <- tibble(mask=round(as.vector(values(sub[['Mask']])), 0),
                 dqf=round(as.vector(values(sub[['DQF']])), 0)) |> 
    mutate(final=case_when(mask==1 & dqf<=qual ~ 1,
                           mask==0 | dqf >qual ~ 0,
                           TRUE ~ NA))

filtro <- filtro |> 
    mutate(id=row_number()) |> 
    filter(final==1)

coords <- xyFromCell(sub, filtro$id) |> 
    as_tibble()

filtro <- filtro |> 
    bind_cols(coords)

plot(pantanal$geometry)
points(filtro$x, filtro$y, pch=20)

# berman e turner
class(pantanal$geometry)
# 2. Converter para SpatVector
v <- vect(pantanal$geometry)

# 3. Extrair os vértices do polígono
coords <- geom(v)[, c("y", "x")]
head(coords)
plot(coords)
# 4. Criar objeto 'owin' a partir das coordenadas
win <- owin(poly = list(x = coords[, 1], y = coords[, 2]))
dado <- ppp(filtro$y, filtro$x, window = win)
dado <- flipxy(dado)
plot(dado)

dado2 <- density(dado, sigma=0.07)
plot(dado2)
plot(dado, add=T)

library(splines)
modelo <- ppm(dado ~ bs(x, df=5) + bs(y, df=5))
predict(modelo)


# Criar um padrão de pontos aleatório em uma janela 100x100
X <- rpoispp(lambda = 100, win = owin(c(0, 10), c(0, 10)))

# Criar uma covariável espacial — por exemplo, uma imagem com gradiente
Z <- as.im(function(x, y) { x + y }, W = Window(X))

# Ajustar o modelo de processo pontual inhomogêneo com covariável Z
modelo <- ppm(X ~ Z, covariates = list(Z = Z))

# Ver o resumo do modelo
summary(modelo)


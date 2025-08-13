# Carregar pacotes necessários
library(fields)
library(sp)

# Malha espacial
x <- seq(0, 1, length = 30)
y <- seq(0, 1, length = 30)
xy <- expand.grid(x = x, y = y)
n <- nrow(xy)

# Covariável simples (região com valor 1, resto 0)
covariavel <- matrix(0, nrow = length(x), ncol = length(y))
covariavel[1:10, ] <- 1
X <- as.numeric(covariavel)

# Função de covariância (Gaussiana)
D <- as.matrix(dist(xy))
C <- exp(-D^2 / 0.1)

# Decomposição espectral para simular GP
eigenC <- eigen(C, symmetric = TRUE)
eigenC$values[eigenC$values < 0] <- 0 # remover pequenos valores negativos

# Simular eta_0 ~ GP(0, C)
set.seed(123)
z0 <- rnorm(n)
eta_0 <- eigenC$vectors %*% (sqrt(eigenC$values) * z0)

# Resposta no tempo 0
beta <- 1
y_0 <- eta_0 + beta * X

# Simular eta_1 ~ GP(phi * eta_0, C)
phi <- 0.4
z1 <- rnorm(n)
eta_1 <- phi * eta_0 + eigenC$vectors %*% (sqrt(eigenC$values) * z1)

# Resposta no tempo 1
y_1 <- eta_1 + beta * X

# Visualizações
par(mfrow = c(2, 2))
fields::image.plot(x, y, matrix(eta_0, nrow = length(x)), main = expression(eta[0]))
fields::image.plot(x, y, matrix(y_0, nrow = length(x)), main = expression(y[0]))
fields::image.plot(x, y, matrix(eta_1, nrow = length(x)), main = expression(eta[1]))
fields::image.plot(x, y, matrix(y_1, nrow = length(x)), main = expression(y[1]))


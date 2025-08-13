require(spatstat)

lonlat <- read.csv("fdcf_2020_183-189_pant.txt", header=FALSE)
colnames(lonlat) <- c("lon","lat")

Pontos <- ppp(lonlat[,2], lonlat[,1], window = owin(c(-59,-54.5), c(-22,-16)))


# Area da caixa:
W <- owin(c(-59,-54.5), c(-22,-16))
area(W)
# Numero de pontos
Pontos$n
# Estimativa taxa (homogenea)
lambda <- Pontos$n / area(W)
lambda

set.seed(1)
X <- rpoispp(lambda, win = W)

plot(Kest(X))

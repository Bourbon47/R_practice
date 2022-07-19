# using bootstrap package

library(bootstrap)
cor.xy <- function(xy) cor(xy[1:n],xy[(n+1):(2*n)])
xy <- c(x,y)

theta <- function(obs,xy.data) cor(xy.data[obs,1],xy.data[obs,2])
xy.data <- matrix(c(x,y),ncol=2)
n <- length(x)
bcanon(1:n,1000,theta,xy.data)
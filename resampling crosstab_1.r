# using bootstrap package

library(bootstrap)

x <- treat
y <- outcome

theta <- function(obs,xy.data) {
   T <- table(xy.data[obs,1],xy.data[obs,2])
    log.psi <- log((T[2,2]*T[1,1])/(T[1,2]*T[2,1]))
    return(log.psi)
}

xy.data <- matrix(c(x,y),ncol=2)
n <- length(x)
bcanon(1:n,1000,theta,xy.data)

# end
# fit weibull distribution to rats data

x <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23)
n <- length(x)

m.log.lik <- function(theta){
    temp1 <- 0; temp2 <- 0
    for (i in 1:n){
      temp1 <- temp1 + log(x[i])
      temp2 <- temp2 + x[i]^theta[1]
    }
    return(-(n*log(theta[1])- n*log(theta[2]) +(theta[1]-1)* temp1 - temp2/theta[2]))
}
nlm(m.log.lik, p = c(1,mean(x)))$estimate

theta <- c(1,mean(x))
optim(theta, m.log.lik)$par

# end
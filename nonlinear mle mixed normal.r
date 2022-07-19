# half-half mixed normal with centers theta-2 and theta+2 with sd 1

set.seed(123)
n <- 100
x <- c(rnorm(n/2,-2,1),rnorm(n/2,2,1))
hist(x,xlim=c(-6,6),freq=F)

log.lik <- function(theta){
     temp <- 0
     for (i in 1:n) temp <- temp + log(0.5*exp(-0.5*(x[i]-theta+2)^2)+0.5*exp(-0.5*(x[i]-theta-2)^2))
     return(temp)
}
x11(); plot(log.lik,-2,2)

log.lik.1 <- function(theta){
     temp <- 0
     for (i in 1:n) {
       temp.1 <- 0.5*(x[i]-theta+2)*exp(-0.5*(x[i]-theta+2)^2)+0.5*(x[i]-theta-2)*exp(-0.5*(x[i]-theta-2)^2)
       temp.2 <- 0.5*exp(-0.5*(x[i]-theta+2)^2)+0.5*exp(-0.5*(x[i]-theta-2)^2)
       temp <- temp + temp.1/temp.2
     }
     return(temp)
}

bisection(log.lik.1, -2,2, 10^-4)

# end
 
# mle for the center of Cauchy distribution

n <- 10
set.seed(123)
x <- rcauchy(n,1)

log.lik <- function(theta) {
   temp <- 0
   for(i in 1:n) temp <- temp - log(1+(x[i]-theta)^2)
   return(temp)
   }
log.lik.1 <- function(theta) {
   temp <- 0
   for(i in 1:n) temp <- temp + 2*(x[i]-theta)/(1+(x[i]-theta)^2)
   return(temp)
   }
log.lik.2 <- function(theta) {
   temp <- 0
   for(i in 1:n) temp <- temp -2*(1-(x[i]-theta)^2)/(1+(x[i]-theta)^2)^2
   return(temp)
   }
plot(log.lik, xlim=c(-5,5), ylim=c(0,10))
abline(v=0, lty="dotted")
x11(); plot(log.lik.1, xlim=c(-5,5), ylim=c(-10,10))
abline(h=0, lty="dotted")
x11(); plot(log.lik.2, xlim=c(-5,5), ylim=c(-10,10))
abline(h=0, lty="dotted")

newton <- function(f,g,x0,epsilon){
   diff <- 1
   while (abs(diff) > epsilon) {
      diff <- -f(x0)/g(x0)
      x0 <- x0 + diff
      }
   return(x0)   
   }

newton(log.lik.1,log.lik.2,median(x),0.5*10^(-4))

uniroot(log.lik.1,c(0,2)) # median(x)

# end
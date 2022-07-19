# numeric double integral for bivariate normal distribution
# integral for 0 < x < 1, 0 < y < 1, x^2 +y ^2 < 1

f <- function(u, v) 1/(2*pi)*exp(-0.5*(u^2+v^2))
n <- 20; n2 <- n*2
x <- (0:n2)/n2
y <- (0:n2)/n2
h <- 1/n

W <- matrix(c(1,4,1,4,16,4,1,4,1),nrow=3)
temp <- 0
for (i in 1:n){
   for (j in 1:n){ 
      r2 <- x[2*i]^2+y[2*j]^2
      if(r2 < 1) {
         row1 <- c(f(x[2*i-1],y[2*j-1]),f(x[2*i-1],y[2*j]),f(x[2*i-1],y[2*j+1]))
         row2 <- c(f(x[2*i+0],y[2*j-1]),f(x[2*i+0],y[2*j]),f(x[2*i+0],y[2*j+1]))
         row3 <- c(f(x[2*i+1],y[2*j-1]),f(x[2*i+1],y[2*j]),f(x[2*i+1],y[2*j+1]))
         F <- rbind(row1,row2,row3)
         temp <-temp+sum(W*F)
      }
   }
}
prob <- temp*(h^2)/36
prob

# iterative integral via integrate()

integrate(function(y) { 
      sapply(y, function(y){
          integrate(function(x)
              1/(2*pi)*exp(-(x^2+y^2)/2),0,sqrt(1-y^2))$value
          })
}, 0, 1)

# cubature package

library(cubature)
f <- function(x) 1/sqrt(2*pi)*exp(-x^2/2)
adaptIntegrate(f, c(0,0), c(1,1))

# end
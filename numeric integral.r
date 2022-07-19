# numeric integral

f <- function(x) 1/sqrt(2*pi)*exp(-x^2/2)

rectangle <- function(f,a,b,n){
    y <- f(a+(b-a)/(2*n)*seq(1,2*n-1,2))
    s <- sum(y)*(b-a)/n
    return(s)
   }

trapezoid <- function(f,a,b,n){
    y <- f(a+(b-a)/(2*n)*seq(0,2*n,2))
    s <- (y[1]+2*sum(y[2:n])+y[n+1])*(b-a)/(2*n)
    return(s)
   }

simpson <- function(f,a,b,n){
    y <- f(a+(b-a)/(2*n)*seq(0,2*n,1))
    temp.1 <- 0; temp.2 <- 0
    for (i in seq(1,2*n-1,2)) temp.1 <- temp.1+y[i+1]
    for (i in seq(2,2*(n-1),2)) temp.2 <- temp.2+y[i+1]
    temp <- f(a)+2*temp.2+4*temp.1+f(b)
    s <- (b-a)/n*temp/6
    return(s)
   }

n <- 20
rectangle(f,0,1,n)
trapezoid(f,0,1,n)
simpson(f,0,1,n)
pnorm(1)-pnorm(0)

f <- function(x) 1/sqrt(2*pi)*exp(-x^2/2)
integrate(f, 0,1)

# end
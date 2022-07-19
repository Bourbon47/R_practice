# linear algebra: transformation
set.seed(123)
x <- cbind(rnorm(100,2,0.5),rnorm(100,3,0.5))
plot(x, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),main="Simulated Data")

# rotation
rotate <- function(x, theta){
   A <- matrix(c(cos(theta),-sin(theta),sin(theta),cos(theta)),2,2,byrow=T)
   return(x %*% t(A))
}
set.seed(123)
x <- cbind(rnorm(100,2,0.5),rnorm(100,3,0.5))
plot(x, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),main="Rotation")
for (k in 1:5){
  x.prime <- rotate(x, k*pi/3)
  par(new=T)
  plot(x.prime, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),xlab="",ylab="")
} 

# reflection
reflect <- function(x, c){
    c <- c / sqrt(sum(c*c))
    K <- matrix(c(c[1]^2-c[2]^2,2*c[1]*c[2],2*c[1]*c[2],c[2]^2-c[1]^2),2,2)
   return(x %*% K)
}
set.seed(123)
x <- cbind(rnorm(100,2,0.5),rnorm(100,3,0.5))
plot(x, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),main="Reflection")
c <- c(-1,2)   # c <- c(2,1)
x.prime <- reflect(x,c)
par(new=T)
plot(x.prime, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),xlab="",ylab="")
abline(c(0,c[2]/c[1]))

# projection
project <- function(x, v){
   v <- v / sqrt(sum(v*v))
   H <- v %*% t(v) 
   return(x %*% H)
}
set.seed(123)
x <- cbind(rnorm(100,2,0.5),rnorm(100,3,0.5))
plot(x, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),main="Projection")
v <- c(-1,2)   # v <- c(2,1)
x.prime <- project(x,v)
par(new=T)
plot(x.prime, xlim=c(-5,5), ylim=c(-5,5),col=rainbow(100),xlab="",ylab="")
abline(c(0,v[2]/v[1]))

# end

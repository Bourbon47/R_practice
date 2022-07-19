# Zeros of the equation by the Newton-Raphson method

newton <- function(f,g,z0,epsilon){
   diff <- 1
   while (abs(diff) > epsilon) {
      if(abs(g(z0)) < epsilon) return("incorrect specification")
      diff <- -f(z0)/g(z0)
      z0 <- z0 + diff
      }
   return(z0)   
   }

h <- function(z) z^5 -8*z^2 + 3
h.1 <- function(z) 5*z^4-16*z

z0 <- 0.4
newton(h,h.1,z0,0.5*10^(-4))

plot(h,0,1,xlim=c(-0.2,1.2),xlab="z",ylim=c(-5,5),ylab="h(z)")
abline(h=0, lty="dotted")
abline(c(h(z0)-h.1(z0)*z0,h.1(z0)),lty="dotted",col="red")
segments(z0,0,z0,h(z0),col="blue")
text(z0,-0.4,"z0")
x11()
z1 <- z0 - h(z0)/h.1(z0)
plot(h,0,1,xlim=c(-0.2,1.2),xlab="z",ylim=c(-5,5),ylab="h(z)")
abline(h=0, lty="dotted")
abline(c(h(z1)-h.1(z1)*z1,h.1(z1)), lty="dotted",col="red")
segments(z1,0,z1,h(z1),col="blue")
text(z1,0.4,"z1")

# end
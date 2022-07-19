# Zeros of the equation by the bisection method

bisection <- function(h,a,b,epsilon){
   y0 <- h(a)
   y1 <- h(b)
   if(y0*y1 > 0) return("incorrect specification")
   else {
      while(b-a > epsilon){
          z <- (a+b)/2
          y <- h(z)
          if(y*y1 < 0) {
              a <- z
              y0 <- y
          }
          else {
              b <- z 
              y1 <- y
          }
      }
      return((a+b)/2)
   }
}

h <- function(z) z^5 -8*z^2 + 3
bisection(h,0,1,0.5*10^(-4))

plot(h,0,1,xlim=c(-0.2,1.2),xlab="z",ylim=c(-5,5),ylab="h(z)")
abline(h=0, lty="dotted")
segments(0,0,0,h(0),lty="dotted",col="blue")
segments(1,0,1,h(1),lty="dotted",col="blue")
segments(0.5,0,0.5,h(0.5),lty="dotted",col="red")
text(0,-0.5,"a")
text(1,0.5,"b")
text(0.45,-0.5,"(a+b)/2")

# end
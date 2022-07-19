# secant Method, example 1

h <- function(z) z^5 - 8*z^2 +3
z.0 <- 0; z.1 <- 1
diff <- z.1-z.0
count <- 0
while(abs(diff) > 0.5*10^-4) {
   z <- z.1 - h(z.1)*(z.1-z.0)/(h(z.1)-h(z.0))
   z.0 <- z.1
   z.1 <- z
   diff <- z.1-z.0
   count <- count+1
}
z
count

# maximization by secant method without derivative, example 2

H <- function(z) z^6/6 - 8*z^3/3 + 3*z
secant.1 <- function(f, x.0, x.1, h, eps) {
   g <- function(f,x,h){
      d1 <- (f(x+h)-f(x-h))/(2*h)
      d2 <- (f(x+h/2)-f(x-h/2))/h
      return(4/3*d2-d1/3)
   }
   temp <- g(f,x.1,h)/(g(f,x.1,h)-g(f,x.0,h))*(x.1-x.0)
   while (abs(temp) > eps){
      x <- x.1 - temp
      x.0 <- x.1
      x.1 <- x
      temp <- g(f,x.1,h)/(g(f,x.1,h)-g(f,x.0,h))*(x.1-x.0)
   }
   return(x)
}
z <- secant.1(H,0,1,0.5*10^-4,10^-4)
round(z,4)

# end
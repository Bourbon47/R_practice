# numeric derivative by Richardson's extrapolation

f <- function(x) sin(x)
f.1 <- function(x, h) (f(x+h)-f(x-h))/(2*h)
h <- 0.1
d.1 <- f.1(0.5, h)
d.2 <- f.1(0.5, h/2)
d.1
d.2
(4*d.2 - d.1)/3

cos(0.5)

g <- deriv(~ sin(x), "x")
x <- 0.5
eval(g)

library(numDeriv)
grad(f, 0.5)

library(mosaic)
g <- D(sin(x) ~ x)
g(x=0.5)

# end


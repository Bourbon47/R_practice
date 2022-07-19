# random normal numbers

mu <- 172
sigma <- 6
x <- rnorm(10000,mu,sigma)
obs <- floor(x)
table(obs %% 2)

# bivariate normal N(0,1) with the correlation rho

u <- rnorm(10000)
v <- rnorm(10000)
rho <- 0.5
x <- u
y <- rho*u + sqrt(1-rho^2)*v
sum((x > 0) & (y > 0))

# end
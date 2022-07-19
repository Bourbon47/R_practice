# finding roots for the equation: x = cos(x)

n <- 50
x <- rep(0,n)
x[1] <- 0.5

for (k in 2:n) x[k] <- cos(x[k-1])
x

diff <- 0.5
eps <- 0.0001/2
k <- 1
while(diff > eps) {
     k <- k+1
     x[k] <- cos(x[k-1])
     diff <- abs(x[k]-x[k-1])
}
k
x[k]

# end
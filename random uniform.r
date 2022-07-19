# uniform numbers

n <- 1000
x <- runif(n)
hist(x, freq=F, nclass=10, main="Uniform", ylim=c(0,1.2))
abline(h =1, col="red", lty="dotted", lwd=2)

# the maximum of uniform numbers

n <- 10
N <- 1000
M <- rep(0,N)
for (i in 1:N){
   u <- runif(n)
   M[i] <- max(u)
}
hist(M, xlim=c(0,1), main="Max of 10 uniform's")
summary(M)[-4]

# arcsin law

N <- 1000
theta <- runif(N)*2*pi
pos.x <- cos(theta)
hist(pos.x, xlim=c(-1,1), main="x-Position on the circle", freq=F)
summary(pos.x)[-4]

# end

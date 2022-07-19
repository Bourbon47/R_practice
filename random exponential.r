# Exponential random numbers

u <- runif(1000)
lambda <- 2
x <- -1/lambda*log(1-u)
round(summary(x),2)
round(summary(rexp(1000,lambda)),2)


# Poisson random numbers

lambda <- 2
N <- 1000
z <- rep(N,0)
for(i in 1:N){
    time <- 0
    count <- 0
    while(time < 1){
        time <- time + rexp(1,lambda)
        count <- count + 1
    }
z[i] <- count - 1
}
sum(z==0)/N
sum(rpois(N,lambda)==0)/N

# end  
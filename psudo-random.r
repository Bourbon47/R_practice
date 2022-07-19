# psudo-random numbers, linear congruential sequence

x <- rep(0,101)
x[1] <- 0
for (i in 2:101){
   x[i] <- (21*x[i-1] + 31) %% 100
   }
x

plot(x[2:101] ~ x[1:100])

# end
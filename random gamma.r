# gamma waiting time

x <- rep(0,1000)
for (i in 1:1000){
    sum <- 0
    for(j in 1:10) sum <- sum + rexp(1,2)
    x[i] <- sum
}
hist(x, nclass=20)
mean(x)
sum(x > 10)

sum(rgamma(1000,10,2) > 10)

# end
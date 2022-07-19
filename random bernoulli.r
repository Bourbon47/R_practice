# Bernoulli process for gambler's ruin

set.seed(124)
k <- 3
max.trial <- 10

count <- 0
n.repeat <- 1000
for (r in 1:n.repeat){
    balance <- k
    trial <- 0
    while((trial < max.trial) & (balance > 0)) {
        trial <- trial+1 
        balance <- balance + 2*rbinom(1,1,0.5)-1
    }
    if(balance > 0) count <- count+1
}
count/n.repeat

# Negative binomial numbers

k <- 3
p <- 0.5
N <- 1000
random.T <- rep(0,N)
for (i in 1:N){
    count <- 0
    t <- 0
    while(count < k) 
+     ifelse(rbinom(1,1,p) > 0.5,count <- count+1,t <- t+1) 
    random.T[i] <- t
}
table(random.T)

# end
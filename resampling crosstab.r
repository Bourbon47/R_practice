# resampling methods for crosstabs
# randomization test

treat <- c(rep(1,83),rep(1,27),rep(2,23),rep(2,13))
outcome <- c(rep(1,83),rep(2,27),rep(1,23),rep(2,13))
addmargins(table(treat,outcome))
row <- apply(table(treat,outcome),1,sum)
round(diag(1/row) %*% table(treat,outcome)*100,1)
chisq.test(table(treat,outcome))

n.repeat <- 1000
cell.rand <- rep(0,n.repeat)
count <- 0
for (k in 1:n.repeat){
    outcome.star <- sample(outcome)
    cell22 <- table(treat,outcome.star)[2,2]
    cell.rand[k] <- cell22
    if (cell22 >= 13) count <- count+1
}
p.value <- count/n.repeat
p.value
hist(cell.rand,nclass=20,xlab="cell.star",main="Randomization Distribution")
segments(12,0,17,0,col="red",lwd=2)

# bootstrap confidence interval

n.repeat <- 1000
logpsi.boot <- rep(0,n.repeat)
n <- length(treat)
T <- table(treat,outcome)
logpsi <- log((T[2,2]*T[1,1])/(T[1,2]*T[2,1]))
round(logpsi,2)

for (k in 1:n.repeat){
    obs.star <- sample(1:n,replace=TRUE)
    treat.star <- treat[obs.star]
    outcome.star <- outcome[obs.star]
    T <- table(treat.star,outcome.star)
    log.psi <- log((T[2,2]*T[1,1])/(T[1,2]*T[2,1]))
    logpsi.boot[k] <- log.psi
}
hist(logpsi.boot,nclass=20,xlab="logpsi.star",xlim=c(-2,2),main="Bootstrap Distribution")
conf.1 <- quantile(logpsi.boot, p=0.025)
conf.2 <- quantile(logpsi.boot, p=0.975)
round(c(conf.1,conf.2),2)
text(conf.1,0,"|",col="red",cex=2)
text(conf.2,0,"|",col="red",cex=2)

# end

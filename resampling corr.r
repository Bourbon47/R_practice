# Correlation: Randomization Test

x <- c(15,26,10,9,15,20,18,11,8,20,7,9,10,11,11,10,12,17,11,10)
y <- c(95,71,83,91,102,87,93,100,104,94,113,96,83,84,102,100,105,121,86,100)
r <- cor(x,y)
r

n.repeat <- 1000
r.random <- rep(0,n.repeat)
count <- 0
set.seed(124)

for(k in 1:n.repeat){
    y.star <- sample(y)
    r.star <- cor(x,y.star)
    r.random[k] <- r.star
    if(r.star <= r) count <- count+1
}
hist(r.random,xlim=c(-1,1),nclass=20,xlab="r.star")
text(r,0,"|",col="red",cex=2)
p.value <- count/n.repeat
p.value

# Correlation: Bootstrap Confidence Interval

n <- length(x)
r.boot <- rep(0,n.repeat)
set.seed(1245)
for(k in 1:n.repeat){
    obs.star <- sample(1:n,replace=T)
    x.star <- x[obs.star]
    y.star <- y[obs.star]
    r.star <- cor(x.star,y.star)
    r.boot[k] <- r.star
}
hist(r.boot,xlim=c(-1,1),nclass=20,xlab="r.star")
conf.1 <- quantile(r.boot, p=0.025)
conf.2 <- quantile(r.boot, p=0.975)
text(conf.1,0,"|",col="red",cex=2)
text(conf.2,0,"|",col="red",cex=2)
round(c(conf.1,conf.2),2)

# end

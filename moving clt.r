# Cental Limit Effect; n = sample size, histogram (frequency)

n <- 10
sec <- 0.2
n.repeat <- 200
set.seed(124)
x <- rep(0,n.repeat)
x11()
for (r in 1:n.repeat){
  u <- runif(n,-1,1)
  z1 <- mean(u)
  # par(new=T)
  hist(x[1:r],freqency=T,ylim=c(0,60),xlim=c(-1,1),xlab="mean",breaks=seq(-1,1,0.1),main=paste("CLT: mean of",n,"observations"))
  x[r] <- z1
  rug(x[1:r],col="blue")
  text(0.9,60,r)
  Sys.sleep(sec)
}

par(new=T)
z <- seq(-1,1,0.1)
m <- 0
s <- sqrt(4/12/n)
p.norm <- pnorm(z,m,s)
d.norm <- (p.norm[2:21]-p.norm[1:20])*n.repeat
z.1 <- seq(-0.95,0.95,0.1)
plot(d.norm~z.1,col="red",type="p",lwd=2,xlim=c(-1,1),xlab="",ylim=c(0,60),ylab="",main="")
# end
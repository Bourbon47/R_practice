# moving pictures demo 3, sleep

n <- 25
sec <- 0.1
theta <- pi/6

par(mar=rep(6,4),bg="darkblue",col.axis="white",col.lab="white",fg="white",lwd=2)
# par(mar=rep(6,4),bg="white",col.axis="darkblue",col.lab="darkblue",fg="darkblue",lwd=2)
color <- sample(rainbow(n))
x <- 0
y <- 0
plot(x,y,type="n",xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",main="Sleep",col.main="White")
circle <- seq(0,2*pi,pi/36)

for (i in 1:n){
r <- 0.5
x.circle <- cos(circle)
y.circle <- sin(circle)
center <- runif(2,-1,1)
radius <- r*runif(n,0,1)
rr <- radius[i]
for (j in 1:9){
    r1 <- rr
#   rr <- rr*(10-j)/(11-j)
    rr <- rr*0.75
    par(new=T)
    plot(center[1]+rr*x.circle,center[2]+rr*y.circle,xlim=c(-1,1),ylim=c(-1,1),type="l",xlab="",ylab="",col=color[i])
    Sys.sleep(sec)
    if(j > 1) {
       par(new=T)
       plot(center[1]+r1*x.circle,center[2]+r1*y.circle,xlim=c(-1,1),ylim=c(-1,1),type="l",xlab="",ylab="",col="darkblue")
    }
    Sys.sleep(sec)
}
}
text(0.5,0.5,"Good Sleep...")
# end
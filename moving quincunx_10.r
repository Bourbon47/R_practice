# quincunx, n=10, p, sec, n.repeat

p <- 0.5
sec <- 0.05
n.repeat <- 100
count <- rep(0,11)

par(yaxt="n",mar=c(2,2,2,2),bg="white")
x1 <- c(-10,0,10); y1 <- c(0,10,0)
plot(x1,y1,xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),type="n",xlab="",ylab="",main=paste("Quincunx (n=10, p=",p,")"))
segments(-10,0,0,10,lty="dotted",col="red")
segments(10,0,0,10,lty="dotted",col="red")
abline(h=0,col="red",lwd=2)

set.seed(123)
for (r in 1:n.repeat){
e <- 2*rbinom(10,size=1,prob=p)-1
x <- c(0,cumsum(e))
y <- 10:0
text(10,10,r-1,col="white")
text(10,10,r)

for (step in 0:10){

if (step > 0) {
    par(new=T)
    plot(x[1:step],y[1:step],xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),xlab="",ylab="",pch=19,cex=1.2,col="white")
}
par(new=T)
plot(x[1:(step+1)],y[1:(step+1)],xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),xlab="",ylab="",pch=c(rep(1,step),19),cex=1.2,col="red")

if (step < 10) {
    segments(x[step+1]-1,y[step+1]-1,x[step+1],y[step+1],lty="dotted")
    segments(x[step+1]+1,y[step+1]-1,x[step+1],y[step+1],lty="dotted")
    }
Sys.sleep(sec)
}

par(new=T)
plot(x[step+1],y[step+1],xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),xlab="",ylab="",pch=19,cex=1.2,col="white")
par(new=T)
plot(x[step+1],y[step+1],xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),xlab="",ylab="",pch=1,cex=1.2,col="red")

count[x[11]/2+6] <- count[x[11]/2+6]+1
par(new=T)
plot(x[11],-10-1/3+(1/3)*count[x[11]/2+6],xlim=c(-11,11),xaxp=c(-10,10,10),ylim=c(-10,10),xlab="",ylab="",pch=19,col="red",cex=1.2)
Sys.sleep(sec)
}

# end
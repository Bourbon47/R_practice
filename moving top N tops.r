# spin a number of tops, visual demo

set.seed(12345678)
# windows(height=5,width=5)
# par(oma=rep(2,4),mar=rep(2,4))

par(mar=rep(6,4),bg="darkblue",col.axis="white",col.lab="white",fg="white",lwd=2)
# par(bg="black",col.axis="white",col.lab="white",fg="white",lwd=2)
sec <- 0.1
n <- 7
plot(c(-1,1),c(-1,1),type="n",xlab="",ylab="",main="Top N Tops",col.main="White")

r <- 0.1
center <- matrix(runif(2*n,-1,1),n,2)
# par(new=T)
# plot(center,col=rainbow(n)[1:n],cex=8,lwd=4,xlim=c(-1,1),ylim=c(-1,1))

for (t in 1:420){
  n.1 <- (t-1) %/% 60 + 1
  for (j in 1:6){
    for (i in 1:n.1){
      theta.0 <- j/3*pi+t/12*pi
      theta.1 <- j/3*pi+(t-2)/12*pi
      par(new=T)
      plot(center[i,1],center[i,2],col=rainbow(n)[i],cex=6,lwd=4,xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",main="Top N Tops",col.main="White")
      segments(center[i,1],center[i,2],center[i,1]+r*cos(theta.0),center[i,2]+r*sin(theta.0),col=rainbow(n)[i],lwd=4)
      segments(center[i,1]+r*cos(theta.0)/10,center[i,2]+r*sin(theta.0)/10,center[i,1]+r*cos(theta.1)*0.9,center[i,2]+r*sin(theta.1)*0.8,col="black",lwd=4)
    }
  }
  # Sys.sleep(0.01)
}
# end

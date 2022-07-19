# it rains 1

n <- 400
sec <- 0.1
theta <- pi/12
par(mar=rep(6,4),bg="white",col.axis="blue",col.lab="blue",fg="blue",lwd=2)
x <- c(-1,-1); y <- c(1,1)
plot(x,y,type="n",xlim=c(-1,1),ylim=c(-1,1),main="It rains...",xlab="",ylab="")
center <- matrix(runif(2*n,-1,1),byrow=T,nrow=n,ncol=2)
length <- rgamma(n,2,10)
start <- cbind(center[,1]+length*0.5*sin(theta),center[,2]+length*0.5*cos(theta))
end <- cbind(center[,1]-length*0.5*sin(theta),center[,2]-length*0.5*cos(theta))

for (i in 1:(n+10)){
    if(i <= n) segments(start[i,1],start[i,2],end[i,1],end[i,2])
    if(i > 10) segments(start[i-10,1],start[i-10,2],end[i-10,1],end[i-10,2],col="white")
    Sys.sleep(sec)
}
par(new=T)
plot(x,y,type="n",xlim=c(-1,1),ylim=c(-1,1))
text(-0.5,-0.5,"End...")

# end
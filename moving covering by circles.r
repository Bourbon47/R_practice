# covering by circles

n <- 100
d <- 0.5
set.seed(123)

x <- runif(n,-2,2)
y <- runif(n,-2,2)
r <- d/2

circle <- matrix(0,nrow=37,ncol=2)

par(mar=rep(6,4),bg="white",col.axis="black",col.lab="black",fg="black",lwd=2)
plot(c(-2,2),c(2,2),xlab="",ylab="",xaxs="i",yaxs="i",xlim=c(-2,2),ylim=c(-2,2),color.axis="white",type="n",main="")

for (j in 1:37){
    theta <- j*2*pi/36
    circle[j,1] <- r*cos(theta)
    circle[j,2] <- r*sin(theta)
}

for (i in 1:n){
    par(new=T)
    plot(cbind(x[i]+circle[,1],y[i]+circle[,2]),xlab="",ylab="",xlim=c(-2,2),ylim=c(-2,2),type="l",xaxs="i",yaxs="i",lwd=2)
#    Sys.sleep(0.1)
}   

count <- 0
for (rr in 1:5000){
    target <- runif(2,-2,2)
    temp <- cbind(x,y)-matrix(rep(target,n),byrow=T,ncol=2)
    cover <- ifelse(min(apply(temp^2,1,sum)) < r^2,1,0)
    count <- count+cover
    color <- ifelse(cover < 0.5,"blue","red")
    par(new=T)
    plot(target[1],target[2],xlab="",ylab="",xlim=c(-2,2),ylim=c(-2,2),xaxs="i",yaxs="i",cex=1.5,pch=19,col=color)
    # Sys.sleep(0.05)

}
count
# mtext(side=1,line=3,paste(paste("count =",count),"/5000"))

# savePlot("covering by circles_1",type="jpg")
# end


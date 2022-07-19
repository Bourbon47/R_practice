# moving pictures demo 5, flowers

n <- 80
sec <- 0.1
# set.seed(123)
# par(mar=rep(6,4),bg="black",col.axis="white",col.lab="white",fg="white",lwd=2)
par(mar=rep(6,4),bg="white",col.axis="black",col.lab="black",fg="black",lwd=2)
color <- sample(rainbow(n))
x <- 0
y <- 0
plot(x,y,type="n",cex=seq(1,10,2),xlim=c(-1,1),ylim=c(-1,1),main="Flowers",xlab="",ylab="",col="white")
   # polygon(matrix(c(-1.1,-1.1,-1.1,-0.55,1.1,0.55,1.1,-1.1),ncol=2,byrow=T),col="green3",border="green3")
   # polygon(matrix(c(-1.1,-0.55,0.5,0.25,-1.1,1.05),ncol=2,byrow=T),col="yellow",border="yellow")

for (i in 1:(n*24)){
    center <- runif(2,-1.1,1.1)
    par(new=T)
    ii <- i %% n + 1
    if (center[2] < 0.5*center[1]) cex.1 <- sample(c(2,2,2,2,2,4,4,4,6,8),1)
    else {
       if (center[2] < -0.5*center[1] + 0.5) cex.1 <- sample(c(1,1,1,2,2,2,2,2,4,4),1)
       else {
         cex.1 <- 1
         plot(center[1],center[2],cex=cex.1,pch="*",xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",col="lightblue",main="Flowers")
       }
    }
}

for (i in 1:(n*4)){
    center <- runif(2,-1.1,1.1)
    par(new=T)
    ii <- i %% n + 1
    if (center[2] < 0.5*center[1]) cex.1 <- sample(c(2,2,2,2,2,4,4,4,6,8),1)
    else {
       if (center[2] < -0.5*center[1] + 0.5) {
          cex.1 <- sample(c(2,2,2,2,2,2,4),1)
          plot(center[1],center[2],cex=cex.1,pch="*",xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",col="green",main="")
       }
    }
}

for (i in 1:n){
    center <- runif(2,-1.1,1.1)
    par(new=T)
    ii <- i %% n + 1
    if (center[2] < 0.5*center[1]) cex.1 <- sample(c(4,6,6,6,6,8,8,10),1)
    else {
       if (center[2] < -0.5*center[1] + 0.5) cex.1 <- sample(c(2,2,2,2,2,2,4),1)
       else cex.1 <- sample(c(1,1,1,1,1,1,2,2,2,2),1)
    }
    plot(center[1],center[2],cex=cex.1,pch="*",xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",col=color[ii],main="")
    Sys.sleep(sec)
}

# savePlot("flowers_2",type="jpg")
# end

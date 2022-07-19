# moving pictures demo 7, fire works

n <- 10
sec <- 0.1

par(mar=rep(6,4),bg="black",col.axis="white",col.lab="white",fg="white",lwd=2)
# par(mar=rep(6,4),bg="white",col.axis="black",col.lab="black",fg="black",lwd=2)

# color <- brewer.pal(7,"Reds")
# color <- brewer.pal(7,"Oranges")
color <- rainbow(7)

x <- 0
y <- 0
plot(x,y,type="n",cex=seq(1,10,2),xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",main="Fireworks",col.main="white")

r <- c(1,1,2,2,2,3,3,4,5,6)

for (i in 1:(n*10)){
    center <- runif(2,-1,1)
    ii <- i %% 7 + 1
    for (j in 1:10){
    if (j == 10) par(new=T)
    plot(center[1],center[2]+(j-1)*0.1,cex=r[j],pch=21,xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",col=color[ii],main="Fireworks",col.main="white")

    Sys.sleep(sec)
    }
       # par(new=T)
       # plot(center[1],center[2],cex=4,pch="*",xlim=c(-1,1),ylim=c(-1,1),xlab="",ylab="",col="black")
    Sys.sleep(sec)
}
# text(0,-0.7,"It is time to close the window.")
# end

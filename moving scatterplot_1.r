# scatterplot matrix and plot3d for ozone data

library(gclus)
data(ozone)
attach(ozone)
pairs(ozone[,c(1,2,5)],col=rainbow(nrow(ozone)))

library(rgl)
plot3d(ozone[,c(1,2,5)],col=rainbow(nrow(ozone)))

# moving scatterplot

Z <- scale(ozone)
y <- Z[,1]; n <- length(y)
x.1 <- Z[,2]
x.2 <- Z[,5]
cuts <- 60
sec <- 0.2
windows(width=8.1,height=8)
corr <- rep(0,cuts+1)

par(mar=c(6,6,6,6))
for (i in 1:(cuts+1)){
    theta <- (i-1)/cuts*pi*2
    x <- cos(theta)*x.1 + sin(theta)*x.2
    plot(x, y, col=rainbow(n), xlim=c(-3,3), ylim=c(-3,3),main="Ozone = Temp + Vis")
    Sys.sleep(sec)
    corr[i] <- cor(x,y)
    color <- ifelse(corr[i]>0,"red","blue")
    mtext(side=4,at=2.7,"+ corr",cex=1.25, line=2)
    mtext(side=4,at=0,"o",cex=1.25, line=2)
    mtext(side=4,at=-2.7,"corr -",cex=1.25, line=2)
    mtext(side=4,at=2*corr[i],"*",cex=4,las=1,line=2,col=color)
}
theta.1 <- (which.max(corr)-1)/cuts*2
round(theta.1,2)
round(c(cos(theta.1*pi),sin(theta.1*pi)),2)

# end
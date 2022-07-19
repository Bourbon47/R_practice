# moving scatterplot of eight X variables for ozone data

library(gclus)
data(ozone)
help(ozone)

X <- scale(ozone[,2:9])
n <- length(y)
p <- ncol(X)

cuts <- 15
sec <- 0.2

for (j in 1:p) {
    y <- X[,j]
    v.order <- order.endlink(cor(X[,-j]))
    v.order.1 <- (1:p)[-j][v.order]
    for (k in 1:(p-2)){
      x1 <- X[,v.order.1[k]]
      k.1 <- k + 1
      x2 <- X[,v.order.1[k.1]]
      for (i in 1:(cuts+1)){
        theta <- (i-1)/cuts*pi/2
        x <- cos(theta)*x1 + sin(theta)*x2
        plot(x, y, col=rainbow(n), xlim=c(-3,3), ylim=c(-3,3), ylab="x")
        text(-2.5,-2.5, v.order.1[k], col="blue", cex=1+2*(cuts-i+1)/cuts)
        text( 2.5,-2.5, v.order.1[k.1], col="red", cex=1+2*(i-1)/cuts)
        text(-2.5,2.5, j,cex=2)
        Sys.sleep(sec)
      }
    }
}

# end
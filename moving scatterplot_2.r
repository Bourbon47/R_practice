# moving scatterplot of y and eight X variables for ozone data

library(gclus)
data(ozone)
help(ozone)

y <- scale(ozone[,1])
X <- scale(ozone[,2:9])
n <- length(y)
p <- 8
v.order <- order.endlink(cor(X))
v.order

cuts <- 15
sec <- 0.2

for (j in 1:(p-1)) {
    x1 <- X[,v.order[j]]
    j.1 <- j + 1
    x2 <- X[,v.order[j.1]]
    for (i in 1:(cuts+1)){
       theta <- (i-1)/cuts*pi/2
       x <- cos(theta)*x1 + sin(theta)*x2
       plot(x, y, col=rainbow(n), xlim=c(-3,3), ylim=c(-3,3))
       text(-2.5,-2.5, v.order[j], col="blue", cex=1+2*(cuts-i+1)/cuts)
       text( 2.5,-2.5, v.order[j.1], col="red", cex=1+2*(i-1)/cuts)
       text(-2.5,2.5, "Y",cex=2)
       Sys.sleep(sec)
    }
}

# end


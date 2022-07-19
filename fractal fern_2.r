# Fractal Fern, No. 2

windows(height=14, width=8)
color <- c("green","green3")
p <- c(0.84,0.91,0.98,1.00)
A1 <- matrix(c(0.95,0.005,-0.005,0.93),byrow=T,nrow=2,ncol=2); b1 <- c(-0.002,0.5)
A2 <- matrix(c(0.035,-0.2,0.16,0.04),byrow=T,nrow=2,ncol=2); b2 <- c(-0.09,0.02)
A3 <- matrix(c(-0.04,0.2,0.16,0.04),byrow=T,nrow=2,ncol=2); b3 <- c(0.083,0.12)
A4 <- matrix(c(0,0,0,0.25),byrow=T,nrow=2,ncol=2); b4 <- c(0,-0.4)
par(mar=c(1,0,4,0))
plot(c(-3,3),c(-2,7),type="n",axes=F,xlab="",ylab="",main="Fractal Fern 2")
x <- c(0,0)
points(x[1],x[2],col=sample(color,1),pch=1,cex=0.25)
for (i in 1:40000){
  r <- runif(1)
  if(r < p[1]) {
    x <- A1%*%x + b1  
    } else if(r < p[2]) {
         x <- A2%*%x + b2
         } else if(r < p[3]) {
              x <- A3%*%x + b3
         } else x <- A4%*%x + b4
    points(x[1],x[2],col=sample(color,1),pch=1,cex=0.25)
    # Sys.sleep(0.01)
}

# end
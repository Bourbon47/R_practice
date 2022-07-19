# Fractal Fern, No. 1

windows(height=14, width=8)
color <- c("green","green3")
p <- c(0.85,0.92,0.99,1.00)
A1 <- matrix(c(0.85,0.04,-0.04,0.85),byrow=T,nrow=2,ncol=2); b1 <- c(0,1.6)
A2 <- matrix(c(0.20,-0.26,0.23,0.22),byrow=T,nrow=2,ncol=2); b2 <- c(0,1.6)
A3 <- matrix(c(-0.15,0.28,0.26,0.24),byrow=T,nrow=2,ncol=2); b3 <- c(0,0.44)
A4 <- matrix(c(0,0,0,0.16),byrow=T,nrow=2,ncol=2)
par(mar=c(1,0,4,0))
plot(c(-3,4),c(-1,11),type="n",axes=F,xlab="",ylab="",main="Fractal Fern 1")
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
         } else x <- A4%*%x 
    points(x[1],x[2],col=sample(color,1),pch=1,cex=0.25)
    # Sys.sleep(0.01)
}

# end
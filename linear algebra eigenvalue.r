# Eigenvalues and eigenvectors of symmetric matrix

A <- matrix(c(1,0.4,0.2,0.4,1,0.4,0.2,0.4,1),byrow=T,nrow=3)

x0 <- as.vector(c(1,0,0))
diff <- 1
eps <- 0.0001
count <- 0

while(diff > eps){
   x1 <- A %*% x0
   lambda <- sqrt(sum(x1*x1)/sum(x0*x0))
   x1 <- x1/sqrt(sum(x1*x1))
   diff <- sqrt(sum((x1-x0)*(x1-x0)))
   x0 <- x1
   count <- count+1
}

round(x0,4)
round(lambda,4)
count

A <- A - lambda*x0%*%t(x0)
# repeat: return to the line "x0 <- as.vector(c(1,0,0))"

# end
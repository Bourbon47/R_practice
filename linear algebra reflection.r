# Reflected images on the sides of triangle

n <- 25
s <- sqrt(3)
V <- matrix(c(0,s/3,-1/2,-s/6,1/2,-s/6,-1,s/3,1,s/3,0,-2/3*s),6,2,byrow=T)

windows(height=6,width=5.5)
plot(V,xlim=c(-1.2,1.2),ylim=c(-3/2,1),xlab="",ylab="")
segments(V[1,1],V[1,2],V[2,1],V[2,2])
segments(V[2,1],V[2,2],V[3,1],V[3,2])
segments(V[3,1],V[3,2],V[1,1],V[1,2])
segments(V[4,1],V[4,2],V[5,1],V[5,2])
segments(V[5,1],V[5,2],V[6,1],V[6,2])
segments(V[6,1],V[6,2],V[4,1],V[4,2])
text(V[1,1],V[1,2]+0.14,"V1")
text(V[2,1]-0.1,V[2,2]-0.1,"V2")
text(V[3,1]+0.1,V[3,2]-0.1,"V3")

# set.seed(1)
X <- matrix(0,n,2)
for (i in 1:n){
   u1 <- runif(1); u2 <- runif(1)
   X[i,1] <- V[2,1]+ u1 + (1-u1)*u2/2
   X[i,2] <- V[2,2] + (1-u1)*u2/2*s
}
row.order <- order(-X[,2])
X <- X[row.order,]
par(new=T)
plot(X,xlim=c(-1.2,1.2),ylim=c(-3/2,1),xlab="",ylab="",col=rainbow(n),cex=2,lwd=2)

# end
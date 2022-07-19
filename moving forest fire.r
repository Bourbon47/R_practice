# Forest Fire, 21*21

par(mar=rep(4,4))
V <- matrix(0,21,21)
V[11,11] <- 1
image(x=1:21, y=1:21, z=V, axes=F, col=c("white","red","black"),breaks=c(-0.5,0.5,1.5,2.5),xlab="",ylab="", main="Fire Process")
abline(h=0.5+(0:21), v=0.5+(0:21))

sec <- 0.5
V.0 <- V
count <- 1
time <- 0
set.seed(124)

while(count > 0) {
   time <- time+1
   for (i in 1:21) {
   for (j in 1:21) {
     if (V.0[i,j]==1)  {
        if((runif(1) > 0.5) & (i <= 20)) if (V.0[i+1,j] == 0) V[i+1,j] <- 1 
        if((runif(1) > 0.5) & (i >= 2)) if (V.0[i-1,j] == 0) V[i-1,j] <- 1 
        if((runif(1) > 0.5) & (j <= 20)) if (V.0[i,j+1] == 0) V[i,j+1] <- 1 
        if((runif(1) > 0.5) & (j >= 2)) if (V.0[i,j-1] == 0) V[i,j-1] <- 1
        V[i,j] <- 2
     } 
   }
   }
   par(new=T)
   image(x=1:21, y=1:21, z=V, axes=F, col=c("white","red","black"),breaks=c(-0.5,0.5,1.5,2.5),xlab="",ylab="", main="")
   abline(h=0.5+(0:21), v=0.5+(0:21))
   burnt <- sum(V==2)
   mtext(side=1,at=11,line=2,burnt)
   Sys.sleep(sec)
   V.0 <- V
   count <- sum(V.0==1)
   mtext(side=1,at=11,line=2,burnt,col="white")
  }
burnt <- sum(V==2)
mtext(side=1,at=20,line=2,paste("burnt = ",burnt))
mtext(side=1,at=2,line=2,paste("time = ",time))
# end


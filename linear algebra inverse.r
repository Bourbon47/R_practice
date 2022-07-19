# Inverse matrix of pxp matrix A

A <- matrix(c(2,-5,4,1,-2,1,1,-4,6),byrow=T,nrow=3,ncol=3)
p <- nrow(A)
E <- diag(rep(1,p))

# solve(A)
# A %*% solve(A)

A.E <- cbind(A,E)
A.E

for (i in 1:p){
    for (j in (i+1):(p+p)) A.E[i,j] <- A.E[i,j]/A.E[i,i]
    A.E[i,i] <- 1
    if (i < p) {
       for (k in (i+1):p) A.E[k,] <- 
                 A.E[k,] - A.E[k,i]/A.E[i,i]*A.E[i,]
    }
}
A.E

X <- matrix(0,p,p)
for (i in p:1){
  if (i < p){
     temp <- rep(0,p)
     for (j in (i+1):p) temp <- temp + A.E[i,j]*X[j,]
     X[i,] <- A.E[i,(p+1):(p+p)] - temp
  }
  else X[i,] <- A.E[i,(p+1):(p+p)]
}

X
A %*% X

# end
# selection sort of a sequence

n <- 10
x <- runif(n)
round(x,2)

for (i in 1:(n-1)){
   for (j in (i+1):n){
      if(x[i] > x[j]) {
         temp <- x[i]
         x[i] <- x[j]
         x[j] <- temp
      }
   }
}
round(x,2)

# end
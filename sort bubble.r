# bubble sort of a sequence

n <- 10
x <- runif(n)
round(x,2)
continue <- 1
for (k in 1:(n-2)){
  while(continue >= 1){
    continue <- 0
    for (i in 1:(n-k)){
      if(x[i] > x[i+1]){
         temp <- x[i]
         x[i] <- x[i+1]
         x[i+1] <- temp
         continue <- 1
      }
    }
  }
}
round(x,2)

# end
# Fibonacci sequence, n >= 3
fibonacci <- function(n, x1, x2){
    temp.1 <- x1
    temp.2 <- x2
    for (k in 3:n){
      temp.3 <- temp.1 + temp.2
      temp.1 <- temp.2
      temp.2 <- temp.3
    }
    return(temp.3)
}
fibonacci(3,1,1)
fibonacci(10,1,1)
fibonacci(20,1,1)

# Fibonacci, cumulatively up to n =50
n <- 50
F <- rep(0,n)
F[1] <- 1
F[2] <- 1
for (k in 3:n) F[k] <- F[k-2]+ F[k-1]
F

# Fibonacci, cumulatively up to n =1000
n <- 1000
F <- rep(0,n)
F[1] <- 1
F[2] <- 1
for (k in 3:n) F[k] <- F[k-2]+ F[k-1]
table(F %/% 10^floor(log(F,10)))

# end
# computation of exp(1)
exponential <- function(x){
    sum <- 1
    temp <- 1
    k <- 1
    while(abs(temp) > .Machine$double.eps){
       temp <- temp*x/k
       sum <- sum+temp
       k <- k+1
    }
    return(sum)
}
print(exponential(1),16)
print(exp(1),16)

# alternative computation of exp(1)
exp.1 <- function(x){
    sum <- 1
    temp <- 1
    for (k in 1:100) {
       if(abs(temp) < .Machine$double.eps) return(sum)
       temp <- temp*x/k
       sum <- sum+temp
    }
    return("Computation is incomplete within the limit 100")
}
print(exp.1(1)^100,16)
print(exp.1(100),16)

# end

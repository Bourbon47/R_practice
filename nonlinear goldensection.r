# golden section search

golden.section <- function(f, x.1, x.4, eps){
  alpha <- (3-sqrt(5))/2
  delta <- x.4-x.1
  while(abs(delta) > eps){
    x.2 <- (1-alpha)*x.1 + alpha*x.4
    x.3 <- alpha*x.1 + (1-alpha)*x.4
    if (f(x.2) > f(x.3)) {
        x.4 <- x.3  
        x.3 <- x.2
        x.2 <- (1-alpha)*x.1 + alpha*x.4
    }
    else {
    x.1 <- x.2
    x.2 <- x.3
    x.3 <- alpha*x.1 + (1-alpha)*x.4
    }
    delta <- x.4-x.1
  }
  return((x.1+x.4)/2)
}

golden.section(log.lik,0,5,0.5*10^-7)

# end
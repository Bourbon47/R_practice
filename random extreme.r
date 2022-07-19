# the maximum of 100 exponential variables

y <- apply(matrix(rexp(1000*100,1/100),1000,100),1,max)
hist(y,nclass=20,probability=T,xlim=c(0,1500),ylim=c(0,0.004),main="Max of 100 Exponentials")

f <- function(z) exp(-(z-100*log(100))/100-exp(-(z-100*log(100))/100))/100
par(new=T)
plot(f,0,1500,ylim=c(0,0.004),lty="dotted",col="blue",xlab="",ylab="")

# end
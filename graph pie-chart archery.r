# pie chart for an archery game

color <- heat.colors(5)
for (j in 1:5){
    if(j >= 2) par(new=T)
    pie(c(1), radius=1-0.2*(j-1), col=color[j],label=NA, border=NA)
    text(1.1-0.2*j,0,j)
}

n <- 10
score <- 0
for (i in 1:n){
    x <- runif(2,-1,1)
    points(x[1],x[2],pch=21,cex=1.2)
    r <- sqrt(sum(x*x))
    score <- score + ifelse(r<1,5-floor(r*5),0)
}
text(-1,-1,"score")
text(-0.75,-1,score)

# end
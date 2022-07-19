# Fractal Triangles 3

x0 <- c(-1.5,1.5); y0 <- c(-1,1.6)
plot(x0,y0,type="n",xlab="",ylab="",main="Triangles Everywhere")
core <- matrix(0,3,2)
list <- matrix(0,243*9,2)
list.1 <- matrix(0,243*9,2)
center <- c(0,0)
core[1,] <- c(0,2/3)
core[2,] <- c(-1/sqrt(3),-1/3)
core[3,] <- c(1/sqrt(3),-1/3)

# polygon(core, col="red")

triangle <- function(center,f){
	p.1 <- center + f*core[1,]
	q.1 <- center + f*core[2,]
	r.1 <- center + f*core[3,]
      polygon(rbind(p.1,q.1,r.1), border="red",lwd=2)
	# segments(p.1[1],p.1[2],q.1[1],q.1[2],col="red",lwd=2)
	# segments(q.1[1],q.1[2],r.1[1],r.1[2],col="red",lwd=2)
	# segments(r.1[1],r.1[2],p.1[1],p.1[2],col="red",lwd=2)
}

triangle(center,1)
for (j in 1:3) list[j,] <- core[j,]

for (i in 1:6){
    count <- 1
    jj <- 3^i
    for (j in 1:jj) {
        triangle(list[j,],0.5^i)
        for (k in 1:3){
            list.1[count,] <- list[j,]+(0.5^i)*core[k,]
            count <- count + 1
        }
    }
    list <- list.1
    Sys.sleep(1)
}


# end


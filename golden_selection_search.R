#golden section search
n <- 10
x <- rcauchy(n,1)
log.lik <- function(theta) {
  temp <- 0
  for(i in 1:n) temp <- temp - log(1+(x[i]-theta)^2)
  return(temp)
}
golden.section <- function(f,x.1,x.4,eps){ #함수 생성
  alpha <- (3-sqrt(5))/2 #alpha의 값은 0.382
  delta <- x.4-x.1 #delta는 황금비
  while(abs(delta)>eps){ #delta의 절댓값이 epsilon보다 큰동안 while문 실행
    x.2 <- (1-alpha)*x.1+alpha*x.4 #x.2는 x1과 x4를 alpha : (1-alpha)로 내분하는 점
    x.3 <- alpha*x.1+(1-alpha)*x.4 #x.3는 x1과 x4를 (1-alpha) : alpha로 내분하는 점
    if(f(x.2)>f(x.3)){ #f(x2)와 f(x3)을 비교하여, f(x2)>f(x3)이면 최대점이 놓이는 구간이 (x1,x3)로 좁혀짐.
      x.4 <- x.3
      x.3 <- x.2
      x.2 <- (1-alpha)*x.1+alpha*x.4
    }
    else { #f(x2)<f(x3)이면 최대점이 놓이는 구간이 (x2,x4)로 좁혀짐.
      x.1 <- x.2
      x.2 <- x.3
      x.3 <- alpha*x.1 + (1-alpha)*x.4
    } #이런 식으로 최대점이 놓이는 구간이 순차적으로 줄어간다.
    delta <- x.4-x.1
  }
  return((x.1+x.4)/2) # 최대값이 놓이는 구간이 충분히 좁혀졌을 때 구간의 중점으로  f(x)가 최대인 곳을 찾음.
}
golden.section(log.lik,0,5,0.5*10^-7) #앞의 코시 분포 중심 사례에 적용.

#fit weibull distribution to rats data
x <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23) #특정처리를 받은 실험쥐들의 수명.
n <- length(x) #x의 길이를 n으로 저장.

m.log.lik <- function(theta){
  temp1 <- 0; temp2 <- 0
  for(i in 1:n){
    temp1 <- temp1+log(x[i]) #temp1에 로그를 취한 x값들을 모두 더해놓음.
    temp2 <- temp2+x[i]^theta[1] #temp2에 x의 세타제곱을 곱한 값을 더해놓음.
  }
  return(-(n*log(theta[1])- n*log(theta[2])
              + (theta[1]-1)* temp1 - temp2/theta[2]))
}
nlm(m.log.lik,p=c(1,mean(x)))$estimate 
#nlm(non-linear minimization)은 뉴튼-랩슨법을 사용해 비선형 최소값을 구하는 것.












#golden section search
n <- 10
x <- rcauchy(n,1)
log.lik <- function(theta) {
  temp <- 0
  for(i in 1:n) temp <- temp - log(1+(x[i]-theta)^2)
  return(temp)
}
golden.section <- function(f,x.1,x.4,eps){ #�Լ� ����
  alpha <- (3-sqrt(5))/2 #alpha�� ���� 0.382
  delta <- x.4-x.1 #delta�� Ȳ�ݺ�
  while(abs(delta)>eps){ #delta�� ������ epsilon���� ū���� while�� ����
    x.2 <- (1-alpha)*x.1+alpha*x.4 #x.2�� x1�� x4�� alpha : (1-alpha)�� �����ϴ� ��
    x.3 <- alpha*x.1+(1-alpha)*x.4 #x.3�� x1�� x4�� (1-alpha) : alpha�� �����ϴ� ��
    if(f(x.2)>f(x.3)){ #f(x2)�� f(x3)�� ���Ͽ�, f(x2)>f(x3)�̸� �ִ����� ���̴� ������ (x1,x3)�� ������.
      x.4 <- x.3
      x.3 <- x.2
      x.2 <- (1-alpha)*x.1+alpha*x.4
    }
    else { #f(x2)<f(x3)�̸� �ִ����� ���̴� ������ (x2,x4)�� ������.
      x.1 <- x.2
      x.2 <- x.3
      x.3 <- alpha*x.1 + (1-alpha)*x.4
    } #�̷� ������ �ִ����� ���̴� ������ ���������� �پ��.
    delta <- x.4-x.1
  }
  return((x.1+x.4)/2) # �ִ밪�� ���̴� ������ ����� �������� �� ������ ��������  f(x)�� �ִ��� ���� ã��.
}
golden.section(log.lik,0,5,0.5*10^-7) #���� �ڽ� ���� �߽� ��ʿ� ����.

#fit weibull distribution to rats data
x <- c(1,1,2,2,3,4,4,5,5,8,8,8,8,11,11,12,12,15,17,22,23) #Ư��ó���� ���� ��������� ����.
n <- length(x) #x�� ���̸� n���� ����.

m.log.lik <- function(theta){
  temp1 <- 0; temp2 <- 0
  for(i in 1:n){
    temp1 <- temp1+log(x[i]) #temp1�� �α׸� ���� x������ ��� ���س���.
    temp2 <- temp2+x[i]^theta[1] #temp2�� x�� ��Ÿ������ ���� ���� ���س���.
  }
  return(-(n*log(theta[1])- n*log(theta[2])
              + (theta[1]-1)* temp1 - temp2/theta[2]))
}
nlm(m.log.lik,p=c(1,mean(x)))$estimate 
#nlm(non-linear minimization)�� ��ư-�������� ����� ���� �ּҰ��� ���ϴ� ��.











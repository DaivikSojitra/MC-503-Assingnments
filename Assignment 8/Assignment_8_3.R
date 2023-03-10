##############################################
########## Program 3 #########################
##############################################

b <- 2
n <- 3

#CDFSamples

f3 <- function(y,b,n){
  return ((1/b)*log(1-(log(1-y)/n)))
}

#MLE For nleq

p3 <- function(x){
  c((50/x[1])+(sum(y)*(1-(x[2]*sum(exp(x[1]*y))))),
    ((50/x[2])-(sum(exp(x[1]*y)))+50))
}

B <- numeric()
N <- numeric()
baisB <- numeric(0)
baisN <- numeric(0)
sqB <- numeric(0)
sqN <- numeric(0)
for(i in 1:1000){
  x <- runif(50)
  y <- f3(x,b,n)
  ini <- c(b,n)
  B[i] <- nleqslv::nleqslv(ini,p3)$x[1]
  N[i] <- nleqslv::nleqslv(ini,p3)$x[2]
  baisB[i] <- B[i] - b
  baisN[i] <- N[i] - n
  sqB[i] <- baisB[i]^2
  sqN[i] <- baisN[i]^2
}

#Mean of MLE
ansB <- mean(B)
ansN <- mean(N)

print(ansB)
print(ansN)

#Bais
mean(baisB)
mean(baisN)

#MSE
mean(sqB)
mean(sqN)

#fisture information matrix

fn1 <- function(x){
  yn <- matrix(nrow = 2,ncol=2)
  yn[1,1] = ((-50/((x[1])^2))-((x[2]+1)*sum((y^x[1])*(log(y)^2)/(1+y^x[1])^2)))
  yn[1,2] = yn[2,1] = (-sum((y^x[1])*(log(y))/(1+y^x[1])))
  yn[2,2] = (-50/x[2]^2)
  return(-yn)
}

o <- c(ansB,ansN)
ans2 <- fn1(o)
print(ans2)

vcm1 <- solve(ans2)
print(vcm1)

#variance covariance matrix
vcm <- matrix(nrow =2, ncol = 2)
vcm[1,1] = var(C)
vcm[1,2] = vcm[2,1] = cov(C,K)
vcm[2,2] = var(K)

print(vcm)
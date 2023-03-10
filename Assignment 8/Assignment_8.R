# Assignment - 8 Submitted by Sojitra Daivik (2211MC15)

##############################################
########## Program 1 #########################
##############################################
rm(list=ls())
library(nleqslv)
#initial guess
c <- 2
k <- 3
#sample CDF
p1 <- function(y,c,k){
  return((((1-y)^(-1/k))-1)^(1/c))
}

#MLE for nleq
fn <- function(x){
  c((50/x[1])+sum(log(y))-(x[2]+1)*sum((y^x[1]*log(y)/(1+y^x[1]))),
    (50/x[2])-sum(log(1+y^x[1])))
}
C <- numeric(0)
K <- numeric(0)
baisC <- numeric(0)
baisK <- numeric(0)
sqC <- numeric(0)
sqK <- numeric(0)
for(i in 1:1000){
  x <- runif(50)
  y <- p1(x,c,k)
  ini <- c(c,k)
  C[i] <- nleqslv(ini,fn)$x[1]
  K[i] <- nleqslv(ini,fn)$x[2]
  baisC[i] <- C[i] - c
  baisK[i] <- K[i] - k
  sqC[i] <- baisC[i]^2
  sqK[i] <- baisK[i]^2
}

#Mean of MLE
  ansC <- mean(C)
  ansK <- mean(K)
  
  print(ansC)
  print(ansK)

#Bais
mean(baisC)
mean(baisK)

#MSE
mean(sqC)
mean(sqK)

#fisture information matrix

fn1 <- function(x){
  yn <- matrix(nrow = 2,ncol=2)
  yn[1,1] = ((-50/((x[1])^2))-((x[2]+1)*sum((y^x[1])*(log(y)^2)/(1+y^x[1])^2)))
  yn[1,2] = yn[2,1] = (-sum((y^x[1])*(log(y))/(1+y^x[1])))
  yn[2,2] = (-50/x[2]^2)
  return(-yn)
}

o <- c(ansC,ansK)
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

intervalC <- c(ansC - 1.96*sqrt(var(C)), ansC + 1.96*sqrt(var(C)))
print(intervalC)
intervalK <- c(ansK - 1.96*sqrt(var(K)), ansK + 1.96*sqrt(var(K)))
print(intervalK)

##############################################
########## Program 2 #########################
##############################################
rm(list=ls())
a <- 2
b <- 4

f2 <- function(y,a,b){
  return((-b/log(y))^(1/a))
}

p2 <- function(x){
  c((50/x[1])-sum(log(y))+(x[2]*sum((y^(-x[1]))*log(y))),
    (50/x[2])-sum(y^(-x[1])))
}

A <- numeric()
B <- numeric()
baisA <- numeric(0)
baisB <- numeric(0)
sqA <- numeric(0)
sqB <- numeric(0)
for(i in 1:1000){
  x <- runif(50)
  y <- f2(x,a,b)
  ini <- c(a,b)
  A[i] <- nleqslv(ini,p2)$x[1]
  B[i] <- nleqslv(ini,p2)$x[2]
  baisA[i] <- A[i] - a
  baisB[i] <- B[i] - b
  sqA[i] <- baisA[i]^2
  sqB[i] <- baisB[i]^2
}

#Mean of MLE
ansA <- mean(A)
ansB <- mean(B)

print(ansA)
print(ansB)

#Bais
mean(baisA)
mean(baisB)

#MSE
mean(sqA)
mean(sqB)

#fisture information matrix

fn2 <- function(x){
  yn <- matrix(nrow = 2,ncol=2)
  yn[1,1] = ((-50/(x[1]^2))-(x[2]*sum((log(y)^2)*(y^(-x[1])))))
  yn[1,2] = yn[2,1] = (sum((log(y))*(y^(-x[1]))))
  yn[2,2] = (-50/x[2]^2)
  return(-yn)
}

o <- c(ansA,ansB)
ans2 <- fn2(o)
print(ans2)

vcm2 <- solve(ans2)
print(vcm2)

#variance covariance matrix
vcm <- matrix(nrow =2, ncol = 2)
vcm[1,1] = var(A)
vcm[1,2] = vcm[2,1] = cov(A,B)
vcm[2,2] = var(B)

print(vcm)

intervalA <- c(ansA - 1.96*sqrt(var(A)), ansA + 1.96*sqrt(var(A)))
print(intervalA)
intervalB <- c(ansB - 1.96*sqrt(var(B)), ansB + 1.96*sqrt(var(B)))
print(intervalB)
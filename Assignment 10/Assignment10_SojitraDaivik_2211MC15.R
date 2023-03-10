# Assignment - 10 Submitted by Sojitra Daivik (2211MC15)
##############################################
########## Program 1 #########################
##############################################
rm(list=ls())
lm = 1.5
mu = 1.2

#Random samples
f1 <- function(y,lm,mu){
  return(((log(1/(1-y))/lm)^(1/2))+mu)
}

#CDF
f2 <- function(x,lm,mu){
  return(1-exp(-lm*((x-mu)^2)))
}

x <- runif(100)
y <- f1(x,lm,mu)
print(y)

ymin <- min(y)
ymax <- max(y)

int <- (ymax - ymin)/10
expected <- numeric(0)
observed <- numeric(0)
i1 <- ymin
i2 <- i1 + int
k=1
while(T){
  count = 0
  expected[k] = f2(i2,lm,mu) - f2(i1,lm,mu)
  for(i in 1:100){
    if(y[i] >= i1 && y[i] <= i2){
      count = count + 1
    }
  }
  observed[k] = count
  i1 <- i2
  i2 <- i2 + int
  k = k +1
  if(i1 > ymax){
    break
  }
}
print(observed)
print(expected)
expected = expected * 100
sum(expected)
sum(observed)
w <- numeric(0)

for(i in 1:length(observed)){
  w[i] = ((observed[i] - expected[i])^2)/expected[i]
}

sumW <- sum(w)
print(sumW)
if(sumW < qchisq(0.95,9)){
  print("Hypothesis is Accepted")
}else{
  print("Hypothesis is Rejected")
}

#MLE For nleq

f3 <- function(x){
  c(((100/x[1])-sum((y-x[2])^2)),
    (2*x[1]*sum(y-x[2])-sum(1/(y-x[2]))))
}
ini <- c(1,1)
B <- nleqslv::nleqslv(ini,f3)$x[1]
N <- nleqslv::nleqslv(ini,f3)$x[2]
print(B)
print(N)


#################################################
########## Program 2 (a)#########################
#################################################
rm(list=ls())
y=c(0.70,0.84,0.58,0.50,0.55,0.82,0.59,0.71,0.72,0.61,0.62,0.49,0.54,0.36,0.36,0.71,0.35,0.64,0.85,0.55,0.59,0.29,0.75,0.46,0.46,0.60,0.60,0.36,0.52,0.68,0.80,0.55,0.84,0.34,0.34,0.70,0.49,0.56,0.71,0.61,0.57,0.73,0.75,0.44,0.44,0.81,0.80,0.87,0.29,0.50)

#MLE
f3 <- function(x){
  c(((2*50)/x[1])-(2*x[1]*sum(y^2))+(x[2]-1)*sum(2*(x[1]^2)*y*exp(-(x[1]*y)^2)/(1-exp(-(x[1]*y)^2))),
    ((50/x[2])+sum(log(1-exp(-(x[1]*y)^2)))))
}
ini <- c(1,1.5)
C <- nleqslv::nleqslv(ini,f3)$x[1]
K <- nleqslv::nleqslv(ini,f3)$x[2]
print(C)
print(K)

#CDF
fn3 <- function(x,C,K){
  return((1-exp(-(C*x)^2))^K)
}

ymin <- min(y)
ymax <- max(y)

int <- (ymax - ymin)/10
i1 <- ymin
i2 <- ymin + int
observed <- numeric(0)
expected <- numeric(0)
k <- 1
while(T){
  count = 0
  expected[k] <- fn3(i2,C,K) - fn3(i1,C,K)
  for(i in 1:50){
    if(y[i] >= i1 && y[i] <= i2){
      count = count + 1
    }
  }
  observed[k] <- count
  k = k +1
  i1 <- i2
  i2 <- i1 + int
  if(i1 > ymax){
    break
  }
}

expected <- expected * 50

w <- numeric(0)

for(i in 1:length(observed)){
  w[i] <- ((observed[i] - expected[i])^2)/expected[i]
}

sumW <- sum(w)
print(sumW)

if(sumW < qchisq(0.95,9)){
  print("Hypothesis is accepted")
} else{
  print("Hypothesis is rejected")
}


#################################################
########## Program 2 (b)#########################
#################################################
rm(list=ls())
library(nleqslv)
y=c(0.70,0.84,0.58,0.50,0.55,0.82,0.59,0.71,0.72,0.61,0.62,0.49,0.54,0.36,0.36,0.71,0.35,0.64,0.85,0.55,0.59,0.29,0.75,0.46,0.46,0.60,0.60,0.36,0.52,0.68,0.80,0.55,0.84,0.34,0.34,0.70,0.49,0.56,0.71,0.61,0.57,0.73,0.75,0.44,0.44,0.81,0.80,0.87,0.29,0.50)


#MLE for nleq
fn <- function(x){
  c((50/x[1])+sum(log(y))-(x[2]+1)*sum((y^x[1]*log(y)/(1+y^x[1]))),
    (50/x[2])-sum(log(1+y^x[1])))
}

ini <- c(5,8)
C <- nleqslv(ini,fn)$x[1]
K <- nleqslv(ini,fn)$x[2]
print(C)
print(K)

#CDF
fn2 <- function(x,C,K){
  return(1-(1+(x^C))^(-K))
}

ymin <- min(y)
ymax <- max(y)

int <- (ymax - ymin)/10
i1 <- ymin
i2 <- ymin + int
observed <- numeric(0)
expected <- numeric(0)
k <- 1
while(T){
  count = 0
  expected[k] <- fn2(i2,C,K) - fn2(i1,C,K)
  for(i in 1:50){
    if(y[i] >= i1 && y[i] <= i2){
      count = count + 1
    }
  }
  observed[k] <- count
  k = k +1
  i1 <- i2
  i2 <- i1 + int
  if(i1 > ymax){
    break
  }
}

expected <- expected * 50

w <- numeric(0)

for(i in 1:length(observed)){
  w[i] <- ((observed[i] - expected[i])^2)/expected[i]
}

sumW <- sum(w)
print(sumW)

if(sumW < qchisq(0.95,9)){
  print("Hypothesis is accepted")
} else{
  print("Hypothesis is rejected")
}
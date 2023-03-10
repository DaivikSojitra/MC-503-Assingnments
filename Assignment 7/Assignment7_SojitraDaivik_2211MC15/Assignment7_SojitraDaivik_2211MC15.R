# Assignment - 7 Submitted by Sojitra Daivik (2211MC15)

##############################################
########## Program 1 #########################
##############################################
#(a)
rm(list=ls())
x <- seq(1, 50)                         
y <- pexp(x, rate = 0.1)
plot(y)
ans = 1/mean(y)
print(ans)

#taken 1/sigma = theta
f1 <- function(x, theta){
  return(-(50*log(theta)-theta*sum(x)))
}

result <- optim(par = 0, f1, x = y, method = "Brent", lower = 0, upper = 3)
result
theta <- result$par
theta

library(MASS)
fitdistr(y, "Exponential")

###############################
#(b)
a <- 2
b <- 0
p <- 0
i <- 0
eq=numeric()
while(i<=50){
  p = -a*log(1-runif(1)) + b
  if(p>b){
    eq[i] = p
    i=i+1
  }
}

f2 = function(x){
  y = -50/x + (sum(eq))/(x^2)
  return (y)
}
Xstr = 2
ansb=nleqslv(Xstr,f2)$x
print(ansb)


##############################################
########## Program 2 #########################
##############################################
library(nleqslv)
b<-2
a<-2

y <- (-b/(log(1-(1-runif(50))^(1/a))))

print(y)

f2 <- function(x){
  c((50/x[1])+sum(log(1-exp(-x[2]/y))),
    (50/x[2])-sum(1/y)+(x[1]-1)*(sum(exp(-x[2]/y)/(y*(1-exp(-x[2]/y))))))
}

xstr <- c(a,b)
ans <- nleqslv(xstr,f2)$x
print(ans)

##############################################
########## Program 3 #########################
##############################################

a <- 1
b <- 2 

y <- (log(1-((1/a)*log(1-runif(50)))))^(1/b)

print(y)

f3 <- function(x){
  c(50*((1/x[1])+1)-sum(exp(y^x[2])),
    (50/x[2])+sum(log(y))-(x[1])*sum(exp(y)*(y^x[2])*log(y))+sum((y^x[2]*log(y))))
}

xstr <- c(a,b)
nleqslv(xstr,f3)
ans <- nleqslv(xstr,f3)$x
print(ans)

##############################################
########## Program 4 #########################
##############################################
#(a)
a <- 2

y <- (a * tan((pi*(runif(50)-0.5))))

print(y)

library(univariateML)
mlcauchy(y)

f4 <- function(a){
  return((50/a)-sum((2*a)/((a^2)+(y^2))))
}

print(nleqslv(2,f4)$x)

#########################
#(b)

a=2
b=3
i <- 1
y <- numeric(0)
while(i <=50){
  F <- runif(1)
    y[i] <- a*tan(pi*F) + b
    i = i+1
}

print(y)
f42 = function(x){
  c(50/x[1] - sum((2*x[1])/((x[1]^2) + ((y-x[2])^2))),
  sum((2*(y - x[2]))/((x[1]^2)+((y-x[2])^2))))
}
xstr = c(2,3)
nleqslv(xstr,f42)
ansA=nleqslv(xstr,f42)$x[1]
ansB=nleqslv(xstr,f42)$x[2]
print(ansA)
print(ansB)

c <- c(1:3)
names(c) <- c("a","b","c")
c[4] <- 4
names(c[4])
b <- c(1:2)
c*b

x <- c(12L,6L,10L)

typeof(median(x))

a <- list("10",TRUE,5.6)

is.numeric(a[1])
is.list(a[1])

ls(pat="^V")
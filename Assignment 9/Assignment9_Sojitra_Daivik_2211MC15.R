##############################################
########## Program 0 #########################
##############################################
#Exponetial Example
rm(list=ls())

x <- seq(1, 50)  
y <- rexp(x, rate = 0.1)

t <- numeric(0)
for(i in 1:50){
  t[i] <- (1-exp(-0.1*y[i]))
}
t = sort(t)
rv <- (1:50)/50

rv2 <- (0:49)/50
print(rv-t)
print(t-rv2)
dmax <- max(rv - t)
dmin <- max(t - rv2)

ans <- max(dmax,dmin)
print(ans)

if (ans < (1.36/sqrt(50))){
  print("Hypothesis is Accepted")
}else {
  print("Hypothesis is Rejected")
}

##############################################
########## Program 1 #########################
##############################################
#(a)
y <- c(.70,.84,.58,.50,.55,.82,.59,.71,.72,.61,.62,.49,.54,.36,.36,.71,.35,.64,.85,.55,.59,.29,.75,.46,.46,.60,.60,.36,.52,.68,.80,.55,.84,.34,.34,.70,.49,.56,.71,.61,.57,.73,.70,.44,.44,.81,.80,.87,.29,.50)

c <- 5.000
k <- 8.2680
cdf <- numeric(0)
for(i in 1:50){
  cdf[i] <- (1-exp(-(c*y[i])^2))^k
}

cdf <- sort(cdf)
rv <- (1:50)/50
``
rv2 <- (0:49)/50
print(rv-cdf)
print(cdf-rv2)
dmax <- max(rv - cdf)
dmin <- max(cdf - rv2)

ans <- max(dmax,dmin)
print(ans)

if (ans < (1.36/sqrt(50))){
  print("Hypothesis is Accepted")
}else {
  print("Hypothesis is Rejected")
}

library(dgof)
ks.test(y,(1-exp(-(c*y)^2))^k)

##############################################
########## Program 1 #########################
##############################################
#(b)
rm(list=ls())
y <- c(.70,.84,.58,.50,.55,.82,.59,.71,.72,.61,.62,.49,.54,.36,.36,.71,.35,.64,.85,.55,.59,.29,.75,.46,.46,.60,.60,.36,.52,.68,.80,.55,.84,.34,.34,.70,.49,.56,.71,.61,.57,.73,.70,.44,.44,.81,.80,.87,.29,.50)

c <- 5.000
k <- 8.2680
cdf <- numeric(0)

for(i in 1:50){
  cdf[i] <- (1-(1+(y[i]^c))^(-k))
}

cdf <- sort(cdf)
rv <- (1:50)/50

rv2 <- (0:49)/50
print(rv-cdf)
print(cdf-rv2)
dmax <- max(rv - cdf)
dmin <- max(cdf - rv2)

ans <- max(dmax,dmin)
print(ans)

if (ans< (1.36/sqrt(50))){
  print("Hypothesis is Accepted")
}else {
  print("Hypothesis is Rejected")
}

library(dgof)
ks.test(y,(1-(1+(y^c))^(-k)))
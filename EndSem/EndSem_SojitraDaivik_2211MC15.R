#################################################
################ Program 1 ######################
#################################################
rm(list=ls())

A <- 0.5
B <- 1.2
R <- 3

A1 <- 0.2
B1 <- 0.5
R1 <- 5

#Survival Function
f1 <- function(x,A,B,R){
  return(1-exp(A*log(x)-(B*(-log(x)^R))))
}

plot(x,f1(x,A,B,R))
plot(x,f1(x,A1,B1,R1))
#Generate Samples
x <- runif(100,0,1)
f1S <- function(x,A,B,R){
  return((x*exp(B*(-log(x)^R)))^(1/A))
}
y <- f1S(x,A,B,R)
print(y)

#################################################
################ Program 2 ######################
#################################################
rm(list=ls())
alpha <- 2
beta <- 1.5

f2pd <- function(x,A,B){
  return(((-1/A)*log(1-x))^(1/B))
}

x <- runif(100)
y <- f2pd(x,alpha,beta)

f2 <- function(x){
  c(((100/x[1])-sum(y^x[2])),((100/x[2])+sum(log(y))-(x[1]*sum((y^x[2])*log(y)))))
}

ini <- c(alpha,beta)
mleAlpha <- numeric(0)
mleBeta <- numeric(0)
biasAlpha <- numeric(0)
biasBeta <- numeric(0)
MSEAlpha <- numeric(0)
MSEBeta <- numeric(0)
for(i in 1:1000){
  mleAlpha[i] <- nleqslv::nleqslv(ini,f2)$x[1]
  mleBeta[i] <- nleqslv::nleqslv(ini,f2)$x[2]
  biasAlpha[i] <- mleAlpha[i] - alpha
  biasBeta[i] <- mleBeta[i] - beta
  MSEAlpha[i] <- biasAlpha[i]^2
  MSEBeta[i] <- biasBeta[i]^2
}

MLEA <- mean(mleAlpha)
MLEB <- mean(mleBeta)

print(MLEA)
print(MLEB)

print(mean(biasAlpha))
print(mean(biasBeta))

print(mean(MSEAlpha))
print(mean(MSEBeta))

#################################################
################ Program 4 ######################
#################################################
rm(list=ls())
y <- c(1.901, 2.132, 2.203, 2.228, 2.257, 2.350, 2.361, 2.396, 2.397,
       2.445, 2.454, 2.474, 2.518, 2.522, 2.525, 2.532, 2.575, 2.614,
       2.616, 2.618, 2.624, 2.659, 2.675, 2.738, 2.740, 2.856, 2.917,
       2.928, 2.937, 2.937, 2.977, 2.996, 3.030, 3.125, 3.139, 3.145,
       3.220, 3.223, 3.235, 3.243, 3.264, 3.272, 3.294, 3.332, 3.346,
       3.377, 3.408, 3.435, 3.493, 3.501, 3.537, 3.554, 3.562, 3.628,
       3.852, 3.871, 3.886, 3.971, 4.024, 4.027, 4.225, 4.395, 5.020)

f4 <- function(x){
  c(((63/x[1])-sum(log(1+exp(-x[2]*y)))),((63/x[2])+((x[1]+1)*sum((x[2]*exp(-x[2]*y))/(1+exp(-x[2]*y))))-sum(y)))
}

#CDF
f4CDF <- function(x,A,L){
  return((1+exp(-L*x))^(-A))
}

ini <- c(2,3)
MLEAlpha <- nleqslv::nleqslv(ini,f4)$x[1]
MLELambda <- nleqslv::nleqslv(ini,f4)$x[2]

#KS-TEST
CDF <- f4CDF(y,MLEAlpha,MLELambda)

CDF <- sort(CDF)

rv <- 1:63/63

rv2 <- 0:62/63

dmax <- max(rv-CDF)
dmin <- max(CDF-rv2)

KS <- max(dmax,dmin)

if(KS < 1.36/sqrt(63)){
  print("Hypothesis is accepted")
} else{
  print("Hypothesis is Rejected")
}

#Chi-Square Test

ymin <- min(y)
ymax <- max(y)

int <- (ymax-ymin)/9

expected <- numeric(0)
observed <- numeric(0)
i1 <- ymin
i2 <- ymin + int
k = 1
while(T){
  count = 0
  expected[k] <- f4CDF(i2,MLEAlpha, MLELambda) - f4CDF(i1,MLEAlpha, MLELambda)
  for(i in 1:63){
    if(y[i]>= i1 && y[i] <= i2){
      count = count + 1
    }
  }
  observed[k] <- count
  i1 <- i2 
  i2 <- i1 + int
  k = k + 1
  if(i1 > ymax){
    break
  }
}

expected <- expected * 63
w <- numeric(0)
for(i in 1:length(observed)){
  w[i] <- ((observed[i]-expected[i])^2)/expected[i]
}
sumW <- sum(w)

if(sumW < qchisq(0.95,8)){
  print("Hypothesis is accepted")
} else{
  print("Hypothesis is Rejected")
}

#################################################
################ Program 5 ######################
#################################################
rm(list=ls())
dataset <- read.csv("C:/R Assignments/EndSem/imdb.csv")

head(dataset)

corMI <- cor(dataset$IMDB_Rating,dataset$Meta_score)

print(corMI)

datasetYearly <- subset(dataset, dataset$Released_Year == "2010")
#Mean
print(mean(datasetYearly$IMDB_Rating))
#variance
print(var(datasetYearly$IMDB_Rating))

datasetChristopher <-  subset(dataset, dataset$Director == "Christopher_Nolan")

#Movies Directed by christopher
print(datasetChristopher$Series_Title)

#Top 10 rated movies
head(dataset[order(dataset$No_of_Votes, decreasing = TRUE), ],10)$Series_Title


#barplot
x <- 2001:2010
moviesIn <- subset(dataset, dataset$Released_Year >= "2001" & dataset$Released_Year <= "2010")

v <- numeric(0)
year <- 2001
i <- 1
while(T){
  count <- 0
  for(j in 1:nrow(moviesIn)){
    if(moviesIn$Released_Year[j] == year){
      count = count +1
    }
  }
  v[i] <- count
  i <- i +1
  year = year+1
  if(year == 2011){
    break
  }
}
barplot(v,names.arg = x, xlab="Years", ylab="No of Movies Released", main = "Barplot Of Movies released to years", col = rainbow(length(x)))


#################################################
################ Program 3 ######################
#################################################
rm(list=ls())

f3 <- function(x,A,B){
  return(1-exp(-(A*x)^B))
}
#(i)
n <- 50
m <- 40

R <- c(10)
for(i in 2:40){
  R[i] <- 0
}
#(ii)
w <- numeric(0)

for(i in 1:m){
  w[i] <- 1
}
#(iii)
v <- numeric(0)

for(i in 1:m){
  v[i] = w[i]^(1/(i+sum(R[m-i+1])))
}
#(iv)
u <- numeric(0)

for(i in 1:m){
  u[i] <- 1 - (v[m]*v[m-1]*v[m-i+1])
}
#(v)

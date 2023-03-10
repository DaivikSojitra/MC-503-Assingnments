####################################################
#######             problem1                 #######
####################################################

#(a)
sigma <- 2
x<-function(y){
  return(-sigma*(log(1-y)))
}

y <- runif(2000,0,1)
ans <- x(y)
print(ans)
print(mean(ans))

#(b)
myu <- 3
sigma <- 2
b<-function(y){
  return(myu-(sigma*(log(exp(myu/sigma)-y))))
}

y=runif(2000,0,1)

print(b(y))
print(mean(b(y)))

####################################################
#######             problem2                 #######
####################################################

#(a)
sigma <- 3
x2a<-function(u){
  return((sigma*tan(pi*(u-0.5))))
}
u=runif(2000,0,1)
print(sort(x2a(u)))
print(mean(x2a(u)))

#(b)
myu <- 5
sigma <- 6
x2b<-function(y){
  return(myu+(sigma*(tan(pi*(y-0.5)))))
}

y=runif(2000,0,1)

print(sort(x2b(y)))
print(mean(x2b(y)))

####################################################
#######             problem3                 #######
####################################################

x2=rep(0,5000)
a=5
for(i in 1:5000)
{
  sample1=runif(1)
  sample2=runif(1)
  sample3=runif(1)
  if(sample1<=a)
  {
    x2[i]=sample2
  }
  else
  {
    x2[i]=max(sample2,sample3)
  }
}
print(mean(x2))
#Actual mean calculation got following
print(-a/6+2/3)
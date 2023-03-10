#problem_1
#(a)
options(max.print=2000)
x<-function(u){
  return(-6*(log(1-u)))
}

for (i in 0:2000) {
  u=runif(i)
}
x(u)
mean(x(u))

#(b)
x1<-function(a){
  return(5-(6*(log(1-a))))
}

for (i in 5:2000) {
  a=runif(i)
}
x1(a)
mean(x1(u))

      #problem 2

#(a)
x2<-function(b){
  return((6*tan(pi*b)))
}
for (i in 1:2000) {
  b=runif(i)
}
x2(b)
mean(x2(b))
#(b)

x3<-function(c){
  return((6*tan(pi*c)+5))
}
for (i in 1:2000) {
  c=runif(i)
}
x3(c)
mean(x3(c))

#problem 3
a=1
  for (i in seq(0,1,0.01)) {
    u1=runif(i)
    u2=runif(i)
    u3=runif(i)
   
  }
print(u1)
print(u2)
if(u1<=1){
  print(u2)
} else {
  print(max(u2,u3))
}
 



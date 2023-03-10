# Assignment - 6 Submitted by Sojitra Daivik (2211MC15)

##############################################
########## Program 1 #########################
##############################################
#bisection

library(NLRoot)
library(pracma)

f1 <- function(x){
  return(x^3-x-4)
}
a =1
b=2

print(bisect(f1,a,b))

#Creating the function
Mybisection <- function(f,a,b,EPSILON){
  if(f(a) * f(b) >= 0){
    print("Wrong selection of a and b")
    return (NULL)
  }
  repeat{
    c = (a+b)/2
    if(f(c) == 0.0){
      break;
    }
    else if(f(a)*f(c) < 0){
      b = c
    }
    else {
      a = c
    }
    if(abs(f(c)) <= EPSILON){
      break
    }
  }
  return (c)
}
ans <- Mybisection(f1,1,2,0.0001)
print(round(ans,3))


##############################################
########## Program 2 #########################
##############################################
#regula-Falsi

f2 <- function(x){
  return(exp(x^2-1)+10*sin(2*x)-5)
}

regulaFalsi(f2,-1,1)

#Creating the function
MyregulaFalsi <- function(f,a,b,N){
  i = 1
  repeat{
    if(f(a) == f(b)){
      print("Mathematical Error")
      return (NULL)
    }
    c = (a*f(b) - b*f(a))/ (f(b) - f(a))
    i = i+1
    if(i>=N){
      print("Not Convergent")
      return ()
    }
    if(f(c) == 0.0){
      break;
    }
    else if(f(a)*f(c) < 0){
      b = c
    }
    else {
      a = c
    }
  }
  return (c)
}
ans2 <- MyregulaFalsi(f2,-1,1,1000)
print(ans2)

##############################################
########## Program 4 #########################
##############################################
#fixedpoint

library(spuRs)

f4 <- function(x){
  return (x^3+4*x^2-10)
}

#f4 = 0 => x = g(x)
g4 <- function(x){
  return (sqrt((10-x^3)/4))
}

fixedpoint(g4,-1,tol = 0.01)

MyFixedPoint <- function(g,a,EPHSILON,Iteration){
  i = 1
  repeat{
    b = g(a)
    cat("At iteration",i," value of x is: ",b, "\n")
    if(abs(b-a) <= EPHSILON){
      print("Algorithm converged")
      return(b)
    }
    a = b
    i = i+1
    if(i > Iteration){
      print("Not Convergent")
      break
    }
  }
}
MyFixedPoint(g4,-1,0.01,10)

##############################################
########## Program 3 #########################
##############################################

f3 <- function(x){
  return (exp(x)-1-x-((x^2)/2)-(((x^3)/6)*exp(0.3*x)))
}

#derivative of f3
g3 <- function(x){
  return (exp(x)-1-x-((1/6)*(0.3*x+1)*(x^2)*exp(0.3*x)))
}

newtonRaphson(f3,2,g3,tol = 0.001)

MynewtonRaphson <- function(f,a,g,EPHSILON,N){
  i = 1
  repeat{
    if(g(a) == 0){
      print("Mathematical Error")
      break
    }
    b = a - (f(a)/g(a))
    i = i+1
    if(i>=N){
      print("Not Convergent")
      break
    }
    if(abs(f(b)) > EPHSILON){
      a = b
    }
    else{
      return (b)
    }
  }
}

print(round(MynewtonRaphson(f3,2,g3,0.001,200),5))

##############################################
########## Program 5 #########################
##############################################

# x - 2y + 3z = 9
# -x + 3y - z = -6
# 2x - 5y + 5z = 17
# create coefficients A and B using given equations

A <- rbind(c(1, -2, 3), 
           c(-1, 3, -1), 
           c(2, -5, 5))
B <- matrix(c(9, -6, 17),3,1)

solve(A, B)

# My Functions
# X = A-1*B => inorder to do this we need to do matrix multiplication and Inverse

# (a) Matrix Multiplication
multiplication <- function(A,B){
  ans <- matrix(c(0),nrow=nrow(A),ncol=ncol(B))
  if(ncol(A) == nrow(B)){
    for(i in 1:nrow(A)){
      for(k in 1:ncol(B)){
        sum <- 0
        for(j in 1:nrow(B)){
          sum <- sum + (A[i,j]*B[j,k])
        }
        ans[i,k] = sum
      }
    }
    return (ans)
  }
  else{
    print("Can not multiply")
  }
}

# (b) Transpose of Matrix
transpose <- function(A){
  r <- nrow(A)
  c <- ncol(A)
  ans <- matrix(0,nrow=c,ncol=r)
  for(i in 1:r){
    for(j in 1:c){
      ans[i,j] = A[j,i]
    }
  }
  return (ans) 
}

#(b) Inverse of Matrix
#1..Minor
Minor <- function(x,i,j){
  y <- matrix(x[-i,-j],nrow = nrow(x)-1, ncol = ncol(x)-1, byrow=T);
  return(det(y))
}

#2..cofactor
Cofactor <- function(x,i,j){
  return(((-1)^(i+j))*Minor(x,i,j))
}

#3..Adjoint
Adjoint <- function(x){
  ans <- matrix(0,nrow(x),ncol(x))
  for(i in 1:nrow(x)){
    for(j in 1:ncol(x)){
      ans[i,j] = Cofactor(x,i,j)
    }
  }
  return(ans)
}

#4..inverse
Inverse <- function(x){
  if(det(x) == 0){
    return ("Inverse does not exists")
  }
  else{
    return ((1/det(x))*Adjoint(x))
  }
}
A1 <- matrix(Inverse(A),ncol = ncol(A),byrow=T)
ans5 <- multiplication(A1,B)
print(ans5)

##############################################
########## Program 6 #########################
##############################################

f6 <- function(x){
  return ((2*exp(-x))-(1/(x+2))-(1/(x+1)))
}

#f6 = 0 => x = g(x)
g6 <- function(x){
  return (-(log(((1/(x+2))+(1/(x+1)))/2,base=exp(1))))
}

#derivative of f6
gd6 <- function(x){
  return ((1/(x+2)^2)+(1/(x+1)^2)-2*exp(-x))
}

#fixedpoint
fixedpoint(g6,1,tol = 0.00001)
print(round(MyFixedPoint(g6,1,0.00001,20),5))

#newtonRaphson
newtonRaphson(f6,1,gd6,tol = 0.00001)
print(round(MynewtonRaphson(f6,1,gd6,0.00001,12),5))

bisect(f6,-3,3)
print(round(Mybisection(f6,-3,3,0.00001),5))

print("If initial guess is proper then newton Raphson is converging faster than fixed point and bisection")
print("No of iterations taken bisection > fixed Point > Newton Raphson ")

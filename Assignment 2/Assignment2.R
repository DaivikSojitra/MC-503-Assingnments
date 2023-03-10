# Assignment 2 - Submitted by Sojitra Daivik (2211MC15)

A <- matrix(c(3,-1,-2,4,1,-2),nrow=2,ncol=3); A
B <- matrix(c(-7,9,2,4,5,-1),nrow=3,ncol=2); B

# Problem 1
# (a) Matrix Multiplication
multiplication <- function(A,B){
  ans <- matrix(0,nrow=nrow(A),ncol=ncol(B))
  if(nrow(A) == ncol(B)){
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
AB <- multiplication(A,B); AB


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
tAB <- transpose(AB);

# (b) Inverse of Matrix
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
  return(transpose(ans))
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
#Execusion order inverse calls Adjoint --> Adjoint calls CoFactor --> Cofactor calls Minor
AA <- matrix(c(3,-1,-2,4,1,-2,1,2,3),nrow=3,ncol=3,byrow = T)
invAB <- Inverse(AB)

# (c) means and variances row and columnwise of A,B,AB,(AB)^t,(AB)^(-1)
#mean_func
mean_func <- function(x){
  sum <- 0
  for(i in 1:length(x)){
    sum <- sum + x[i]
  }
  return (sum/length(x))
}

#mean_RowWise_ColumnWise
mean_row_Col <- function(x){
  nrow <- nrow(x)
  ncol <- ncol(x)
  print("Mean rowWise")
  for(i in 1:nrow){
    print(mean_func(x[i,]))
  }
  print("Mean Colwise")
  for(i in 1:ncol){
    print(mean_func(x[,i]))
  }
}

mean_row_Col(A)
mean_row_Col(B)
mean_row_Col(AB)
mean_row_Col(invAB)
mean_row_Col(tAB)

#standard_deviation

standardDeviation <- function(x){
  sum <- 0;
  mean1 <- mean_func(x)
  for(i in 1:length(x)){
    sum <- sum + (x[i]-mean1)^2
  }
  n <- length(x)-1
  ans <- sum/n
  return (sqrt(ans))
}

standardDeviation_row_col <- function(x){
  nrow <- nrow(x)
  ncol <- ncol(x)
  print("Standard Deviation rowWise")
  for(i in 1:nrow){
    print(standardDeviation(x[i,]))
  }
  print("Standard Deviation ColumnWise")
  for(i in 1:ncol){
    print(standardDeviation(x[,i]))
  }
}

standardDeviation_row_col(A)
standardDeviation_row_col(B)
standardDeviation_row_col(AB)
standardDeviation_row_col(invAB)
standardDeviation_row_col(tAB)

# Problem 2 - Factorial of given number
factorial <- function(x){
  if (x==0 || x == 1){
    return (1);
  }
  return (x * factorial(x-1))
}

factorial(2)

# Problem 3 - find maximum and minimum from a set of numbers
X1 <- c(-4, 44, 7, 2, 40, 54, 1, -3, 4)
MaxMin <- function(x){
  max <- x[1]
  min <- x[1]
  for(i in 1:length(x)){
    if(x[i]>max){
      max <- x[i]
    }
    if(x[i]<min){
      min <- x[i]
    }
  }
  return (c(max,min))
}
MaxMin(X1)

# Problem 4 - Sort the given numbers
BubbleSort <- function(x){
  for(i in 1:length(x)){
    for(j in i:length(x)){
      if(x[i] > x[j]){
        temp <- x[i]
        x[i] <- x[j]
        x[j] <- temp
      }
    }
  }
  return (x)
}
BubbleSort(X1)

# Problem 5 - Check whether a number prime or com-posite
checkPrime <- function(x){
  for(i in 2:(x/2)){
    if(x %% i == 0){
      return ("Number is Composite")
    }
  }
  return ("Number is prime")
}

checkPrime(15)

# Problem 6 - Implement Gamma Function
x2 <- c(8,2,25,3/2)
gamma_function <- function(x){
  if(x == 0.5){
    return (pi**0.5)
  }
  if(x == 1){
    return (1)
  }
  return ((x-1)*gamma_function(x-1))
}
gamma_function(3/2)

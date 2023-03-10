#Assignment 3 : Submitted By : Sojitra Daivik : 2211MC15

#Simpson 1/3 
simpson <- function(a,b,n,f){
  
  h <- (b - a) / n
  x <- seq(a,b,by = h)

  ans <- ((h / 3) * (f(x[1]) + f(x[length(x)])+ 4*sum(f(x[seq.int(2,length(x)-1,2)])) + 2*sum(f(x[seq.int(3,length(x)-1,2)])) ))
  
  return(ans)
}

#Trapezoidal
trapezoidal <- function(a,b,n,f){
  h <- (b-a)/n
  x <- seq(a,b,by = h)
  
  ans <- ((h/2)*(f(a)+f(b)+ 2* sum(f(x[seq(2,length(x)-1,1)]))))
  
  return (ans)
}


#we need to pass functions as parameter to simpson and trapezoidal so creating each problem as function
problem1 <- function(x){
  return((4*x) - (3*(x^2)))
}

problem2 <- function(x){
  return (1/(1+x))
}

problem3 <- function(x){
  return ((x)/(1+x))
}

problem4 <- function(x){
  return (x+(1/x))
}

problem5 <- function(x){
  return (exp(x))
}

problem6 <- function(x){
  return ((exp(x)*sin(x)))
}

#First problem
simpson(0,1,10,problem1)
trapezoidal(0,1,10,problem1)

#Second problem h=1 is given so n would be n=5
trapezoidal(0,5,5,problem2)

#third problem
formatC(simpson(0,5,6,problem3),digits = 3,format="f")
formatC(trapezoidal(0,5,6,problem3),digits = 3,format="f")

#fourth problem
formatC(simpson(1.2,1.6,4,problem4),digits = 2,format="f")
formatC(trapezoidal(1.2,1.6,4,problem4),digits = 2,format="f")

#fifth problem
formatC(simpson(0,0.6,6,problem5),digits = 5,format="f")
formatC(trapezoidal(0,0.6,6,problem5),digits = 5,format="f")

#sixth problem
formatC(simpson(0,pi/2,6,problem6),digits = 5, format ="f")
formatC(trapezoidal(0,pi/2,6,problem6),digits = 5, format ="f")

#Just to verify integration is proper or not
integrate(problem1,0,1)$value
integrate(problem2,0,5)$value
integrate(problem3,0,5)$value
integrate(problem4,1.2,1.6)$value
integrate(problem5,0,0.6)$value
integrate(problem6,0,pi/2)$value
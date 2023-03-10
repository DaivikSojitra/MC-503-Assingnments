####################################################
#######             problem1                 #######
####################################################
rm(list=ls())
AQI <- c(276, 7, 92, 268, 412, 86)
City <- c("Patna", "Ratlam", "Mysore", "Jaunpur", "Pitampura", "Panchkula")
piepercent<- round(100*AQI/sum(AQI), 1)

pie(AQI,piepercent, main="AQI chart of cities", col=rainbow(length(AQI)))

legend(x = "topleft", box.col = "brown",
       bg ="yellow", box.lwd = 2 , title="Cities", 
       legend=City,
       cex = 0.3,
       fill = rainbow(length(AQI)))

chart <- pie3D(AQI, radius = 2.3, height = 0.2, explode = 0.2,main = "AQI chart of cities")
pie3D.labels(chart,radius=2.4,height=0.1,labels = City, labelcex = 0.5)


####################################################
#######             problem2                 #######
####################################################

Runs <- c(12,0,35,35,18,33,1)
Balls <- c(18,1,34,29,18,17,1)
Players <- c("Rohit","KL Rahul", "Kohli", "Jadeja", "Surya", "Pandya", "D Karthik")
barplot(Runs, names.arg=Players,xlab="Players",ylab="Runs",col="cyan",
        main="Players Performance")
print(cov(Runs,Balls))
print(cor(Runs,Balls))

####################################################
#######             problem3                 #######
####################################################

RunsE = c(6,29,73,11,1,91,85,31,0,4,0)
BallsE = c(9,28,173,48,6,58,138,91,12,11,2)
Fours <- c(1,5,11,0,0,9,12,2,0,1,0)
legA <- c("Runs","Balls","Fours")
mat <- matrix(c(RunsE,BallsE,Fours), ncol = length(RunsE), byrow = T)
PlayersE = c("Rohit", "Subhmam", "Puara", "Kohli", "Rahane", "Pant", "Washigton", "Ashwin", "Nadeem", "Ishant", "Bumrah")

barplot(mat,names.arg = PlayersE, xlab="Player Names",ylab="Performance",col=rainbow(length(PlayersE)),
        main="Indian Player's Score")

legend(x = "topright", box.col = "brown",
       bg ="yellow", title="Chart", 
       legend=legA, 
       cex = 0.5,
       fill = rainbow(length(PlayersE)))


####################################################
#######             problem4                 #######
####################################################

prob1 <- function(x){
  return(sin(x^3 + 5*x))
}

val <- c(0)
xval <- seq(-2,5,by=0.01)
val <- prob1(seq(-2,5,by=0.01))
plot(xval, val,type = 'l')


prob2 <- function(x){
  return(cos(abs(x) + exp(x^2)))
}

val <- c(0)
val <- prob2(seq(-2,5,by=0.01))
lines(xval, val, col = "Green")

prob3 <- function(x){
  return(prob1(x) + prob2(x))
}

val <- c(0)
val <- prob3(seq(-2,5,by=0.01))
lines(xval, val, col = "Red")

leg = c("sin(x^3 + 5*x)", "cos(abs(x) + exp(x^2))", "prob1(x) + prob2(x)")

legend(x= "topright", legend=leg, bg = "Yellow", fill = c("black","Green","Red"), cex = 0.3)


####################################################
#######             problem5                 #######
####################################################

data("mtcars")
head(mtcars)

# Plotting the chart
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")

####################################################
#######             problem6                 #######
####################################################

randomNum <- sample(0:100, 50, replace=T)
print(randomNum)
hist(randomNum,xlab = "Level",col = "yellow",border = "blue",xlim = c(20,100), ylim = c(0,10),breaks = 20)

####################################################
#######             problem7                 #######
####################################################

problem7 <- function(x){
  return ((sin(x^2)) + (4*cos(x)))
}

#plot background structure of entire interval in white
xval <- seq(0,2*pi,by = 0.01)
val <- problem7(seq(0,2*pi,by = 0.01))
plot(xval, val, type= 'h', col = "white", main = "function (sin(x^2)) + (4*cos(x))")

#plot first interval in red
xval1 <- seq(0,pi/2,by = 0.01)
val1 <- problem7(seq(0,pi/2,by = 0.01))
points(xval1, val1, type= 'h', col = "Red")

#plot second interval in Green
xval2 <- seq(pi/2,pi,by = 0.01)
val2 <- problem7(seq(pi/2,pi,by = 0.01))
points(xval2,val2,col='Green', type = 'h')

#plot third interval in Blue
xval3 <- seq(pi,(3*pi)/2,by = 0.01)
val3 <- problem7(seq(pi,(3*pi)/2,by = 0.01))
points(xval3,val3,col='Blue', type = 'h')

#plot fourth interval in Gold
xval4 <- seq((3*pi)/2,2*pi,by = 0.01)
val4 <- problem7(seq((3*pi)/2,2*pi,by = 0.01))
points(xval4,val4,col='Gold', type = 'h')

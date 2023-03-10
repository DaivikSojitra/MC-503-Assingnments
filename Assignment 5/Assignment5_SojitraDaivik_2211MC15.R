####################################################
#######             problem1                 #######
####################################################
rm(list=ls())
library(MASS)
head(iris)

#i
print(nrow(iris))
print(ncol(iris))

#ii
print(summary(iris$Sepal.Length))
print(summary(iris$Sepal.Width))

#iii
print(summary(iris$Species))

#iv
x <- subset(iris, Petal.Length > 2)
my_DataFrame <- data.frame(x)
print(my_DataFrame)


####################################################
#######             problem2                 #######
####################################################
#5 ROW -> 4 Col

Index <- 1:5
random <- sample(1:50,5)
Health <- c(90,80,70,100,79)
Animals <- c("Lion","Tiger","Cat","Dog","Cow")
data_frame <- data.frame(Index,random,Health,Animals)
print(data_frame)

####################################################
#######             problem3                 #######
####################################################

library(qpcR) #to include NA in last removed outlier frame in cbind
x1 <- c(2,4,6,10,4,7,12,20,5)
x2 <- c(10,5,5,20,4,70,40,12,NaN)
x3 <- c(2,4,2.5,34,1.6,9.5,6,2,NaN)
prob3_data_frame <- data.frame(x1,x2,x3)

iqrx1 <- IQR(prob3_data_frame$x1)
print(iqrx1)
removedx1 <- subset(prob3_data_frame$x1, prob3_data_frame$x1 > (summary(prob3_data_frame$x1)[["1st Qu."]] - 1.5*iqrx1) & prob3_data_frame$x1 < (summary(prob3_data_frame$x1)[["3rd Qu."]] + 1.5*iqrx1))
print(removedx1)

iqrx2 <- IQR(prob3_data_frame$x2, na.rm = T)
print(iqrx2)
removedx2 <- subset(prob3_data_frame$x2, prob3_data_frame$x2 > (summary(prob3_data_frame$x2)[["1st Qu."]] - 1.5*iqrx2) & prob3_data_frame$x2 < (summary(prob3_data_frame$x2)[["3rd Qu."]] + 1.5*iqrx2))
print(removedx2)

iqrx3 <- IQR(prob3_data_frame$x3, na.rm = T)
print(iqrx3)
removedx3 <- subset(prob3_data_frame$x3, prob3_data_frame$x3 > (summary(prob3_data_frame$x3)[["1st Qu."]] - 1.5*iqrx3) & prob3_data_frame$x3 < (summary(prob3_data_frame$x3)[["3rd Qu."]] + 1.5*iqrx3))
print(removedx3)

removed_outlier <- qpcR:::cbind.na(removedx1,removedx2,removedx3)
removed_outlier_dataFrame <-  data.frame(removed_outlier)
print(removed_outlier_dataFrame)

####################################################
#######             problem4                 #######
####################################################

marks <- c(sample(20:25,5),sample(26:30,4),sample(31:35,3),sample(36:40,4),sample(41:45,2),sample(46:50,1))
numStudent <-  seq(from = 20, to = 45, by = 5)
print(marks)
hist(marks, xlab = "Marks in Statistics",ylab = "Number of students",col = "Red",border = "blue", ylim = c(1,5), breaks = c(20, 25, 30, 35, 40, 45, 50))
x <- c(20,25,30,35,40,45,50)
y <- c(5,4,3,4,2,1)

statmean <- function(x,y){
  tempsum <- c(NULL)
  #calculated (x1 + x2/2)*y for mean calculation
  for(i in 1:length(x)-1){
    tempsum <- append(tempsum, ((x[i] + x[i+1])/2)*y[i])
  }
  return (sum(tempsum)/sum(y))
}

statMed <- function(x,y){
  freqSum <- c(NULL)
  sumq <- 0
  for(i in 1:length(y)){
    sumq <- sumq + y[i]
    freqSum <- append(freqSum,sumq)
  }

  N <- sum(y)/2
  i <- 1
  while(TRUE){
    if(freqSum[i] > N){
      C <- freqSum[i-1]
      L <- x[i]
      f <- y[i]
      break
    }
    i = i+1
  }
  return (L + ((N-C)/f)*5)
}

statMod <- function(x,y){
  freqSum <- c(NULL)
  sumq <- 0
  for(i in 1:length(y)){
    sumq <- sumq + y[i]
    freqSum <- append(freqSum,sumq)
  }
  
  N <- sum(y)/2
  i <- 1
  while(TRUE){
    if(freqSum[i] > N){
      f1 <- y[i]
      f0 <- y[i-1]
      f2 <- y[i+1]
      L <- x[i]
      break
    }
    i = i+1
  }
  return (L + ((f1-f0)/((2*f1)-f0-f2))*5)
}

print(statmean(x,y))
print(statMod(x,y))
print(statMed(x,y))

####################################################
#######             problem5                 #######
####################################################

data <- read.csv("C:/R Assignments/Assignment 5/medals_total.csv")

#i
print(data[data$Country == "India",c("Gold.Medal","Silver.Medal","Bronze.Medal","Country")])
print(data[data$Country == "United States of America",c("Gold.Medal","Silver.Medal","Bronze.Medal","Country")])
print(data[data$Country == "People's Republic of China",c("Gold.Medal","Silver.Medal","Bronze.Medal","Country")])

#ii
chinaMadels <- c(data[data$Country == "People's Republic of China","Gold.Medal"], data[data$Country == "People's Republic of China","Silver.Medal"], data[data$Country == "People's Republic of China","Bronze.Medal"])
ukMedals <- c(data[data$Country == "Great Britain","Gold.Medal"], data[data$Country == "Great Britain","Silver.Medal"], data[data$Country == "Great Britain","Bronze.Medal"])
histChina <- c(NULL)
histUk <- c(NULL)
for(i in 1:length(chinaMadels)){
  histChina = append(histChina, rep(chinaMadels[i], times=chinaMadels[i]))
}

for(i in 1:length(ukMedals)){
  histUk = append(histUk, rep(ukMedals[i], times=ukMedals[i]))
}
print(histUk)
hist(histChina, xlab="Different Medals", ylab = "Number of Medals", main = "Medals won by China")
hist(histUk, xlab="Different Medals", ylab = "Number of Medals", main = "Medals won by UK")

#iii
filteredData <- rbind(data[data$Country == "India",],data[data$Country == "United States of America",],data[data$Country == "Japan",],data[data$Country == "People's Republic of China",],data[data$Country == "Brazil",])
dataframefilteredData <- data.frame(filteredData)
print(dataframefilteredData)

#iv
pie(dataframefilteredData$Gold.Medal,dataframefilteredData$Country,main="Gold medals earned by countries",col=rainbow(length(dataframefilteredData$Country)))
pie(dataframefilteredData$Silver.Medal,dataframefilteredData$Country,main="Silver medals earned by countries",col=rainbow(length(dataframefilteredData$Country)))
pie(dataframefilteredData$Bronze.Medal,dataframefilteredData$Country,main="Bronze medals earned by countries",col=rainbow(length(dataframefilteredData$Country)))
pie(dataframefilteredData$Total,dataframefilteredData$Country,main="Total medals earned by countries",col=rainbow(length(dataframefilteredData$Country)))

####################################################
#######             problem6                 #######
####################################################

print(AirPassengers)
totalPass <- 0;
# (i) AS we are having data from 1949 to 1960 we can directly add
for(i in 1:length(AirPassengers)){
  totalPass <- totalPass + AirPassengers[i]
}
print(totalPass)

#ii
df_AirPass <- data.frame(AirPassengers)
sumYears <- matrix(AirPassengers, ncol = 12, byrow = T)

yearwisePassengers <- c(rowSums(sumYears[,]))
print(yearwisePassengers)

years = seq(1949,1960, by =1)
plot(years,yearwisePassengers, xlab = "Year", ylab = "Number of Passengers")

#iii
df <- data.frame(sumYears[,1],sumYears[,2],sumYears[,3], sumYears[,4], sumYears[,5], sumYears[,6], sumYears[,7], sumYears[,8], sumYears[,9], sumYears[,10], sumYears[,11], sumYears[,12])
boxplot(df, main = "BoxPlot for Months with passengers", xlab = "Months", ylab = "Passengers")
